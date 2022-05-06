use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_error::abort_call_site;
use quote::{format_ident, quote, ToTokens};
use std::env::var;
use std::fmt::Write as _;
use std::fs::{create_dir_all, File};
use std::io::Write;
use std::path::PathBuf;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Brace, Comma, Pub},
    Expr, ExprLit, Field, Fields, Ident, ItemEnum, Lit, MetaNameValue, PathArguments, PathSegment,
    Variant, VisPublic,
};

/// This macro is applied to Rust enums. It generates code that will expose the enum to TypeScript as a discriminated union. It uses `napi` to accomplish this.
/// Enunion also handles automatically converting between the two representations, in Rust you can define `#[napi]` methods that accept the enum as an argument, or return an instance of that enum.
/// The TypeScript will be free to handle it as a discriminated union, the Rust can handle it as an enum, and enunion will take care of translating at the boundary.
///
/// **This macro will not work if `napi` and `napi_derive` are not specified in the `[dependencies]` section of the `Cargo.toml`.**
///
/// This macro has a companion program called `enunion-post-build` which must be executed in the directory after `napi build` has been called. Otherwise the resulting TypeScript will not compile.
/// One simple way to ensure this happens is with a `"postbuild"` script in `package.json` like so
///
/// ```json
/// "scripts": {
///   "build": "napi build --platform --release",
///   "postbuild": "enunion-post-build",
/// },
/// ```
///
/// To install `enunion-post-build` execute this
///
/// ```ignore
/// cargo install enunion-post-build
/// ```
///
/// # Params
/// - `discriminant_repr`: The representation used by the discriminant field, one of "enum", "enum_str", "i64", "none", or "str". Default: "i64"
/// - `discriminant_field_name`: The name of the discriminant field. Can be overridden here if you don't like the default. Default: `<enum_name>_type`, where `<enum_name>` is the name of your enum.
/// This will be converted to lowerCamelCase in the TypeScript file.
///
/// # Discriminant Representation types
///
/// - "enum" - Generates a companion enum and uses variants from it to discriminate the union.
/// - "enum_str" - Generates a companion string enum and uses variants from it to discriminate the union.
/// - "i64" - Uses non-negative whole numbers to discriminate the union variants.
/// - "str" - Uses strings to discriminate the union variants
/// - "none" - No discriminant is used. The type is inferred from the fields present. If this is used
/// then no two variants should share a structure. Variants will be tried top to bottom, and the
/// first one that succeeds will be returned.
///
/// # Example Invocations
/// `#[enunion::enunion]`
///
/// `#[enunion::enunion(discriminant_repr = "str")]`
///
/// `#[enunion::enunion(discriminant_field_name = "my_type")]`
///
/// `#[enunion::enunion(discriminant_repr = "str", discriminant_field_name = "my_type")]`
///
/// # Example
///
/// ```
/// #[enunion::enunion]
/// pub enum Foo {
///     Bar,
///     Baz {
///         a: i32,
///         b: u32,
///         c: String,
///         my_multi_word_field: i32,
///     }
/// }
///
/// #[napi]
/// pub fn take_foo(f: Foo) -> Foo {
///    assert!(matches!(f, Foo::Baz {a: 3, b: 2, c: _, my_multi_word_field: 2}));
///     match f {
///         Foo::Baz {c, ..} => assert_eq!(c, "Hello World"),
///         _ => unreachable!(),
///     }
///     Foo::Baz {
///         a: 1,
///         b: 2,
///         c: String::from("yo"),
///         my_multi_word_field: 8,
///     }
/// }
/// ```
#[proc_macro_error::proc_macro_error]
#[proc_macro_attribute]
pub fn enunion(attr_input: TokenStream, item: TokenStream) -> TokenStream {
    let attr: Args = syn::parse(attr_input).unwrap_or_else(|e| abort_call_site!("Failed to parse enunion input, please use field = \"value\" in a comma separated list. Check the documentation for examples. Error: {:?}", e));
    let mut repr = None;
    let mut discriminant_field_name = None;
    for MetaNameValue { path, lit, .. } in attr.items.iter() {
        match path.get_ident().map(|i| i.to_string()).as_deref() {
            Some("discriminant_repr") => match lit {
                Lit::Str(s) => {
                    match s.value().as_str() {
                        "i64" => {
                            repr = Some(DiscriminantRepr::I64);
                        }
                        "str" => {
                            repr = Some(DiscriminantRepr::String);
                        }
                        "enum" => {
                            repr = Some(DiscriminantRepr::Enum);
                        }
                        "enum_str" => {
                            repr = Some(DiscriminantRepr::EnumStr);
                        }
                        "none" => {
                            repr = Some(DiscriminantRepr::None);
                        }
                        other => {
                            abort_call_site!("{} is not a recognized representation, please use \"i64\", \"enum\", \"enum_str\", \"none\", or \"str\"", other)
                        }
                    }
                }
                _ => abort_call_site!("only string literals are supported for the discriminant_repr, please provide \"i64\", \"enum\", \"enum_str\", \"none\", or \"str\"")
            },
            Some("discriminant_field_name") => 
                match lit {
                    Lit::Str(s) => {
                        discriminant_field_name = Some(s.value());
                    }
                    _ => abort_call_site!("only string literals are supported for the discriminant_field_name, please provide a string")
                },
            _ => {
                abort_call_site!("{} is not a recognized argument, only discriminant_repr, and discriminant_field_name are recognized.", path.to_token_stream());
            }
        }
    }

    let e: ItemEnum = syn::parse(item).unwrap_or_else(|e| {
        abort_call_site!(
            "enunion only supports enums, do not use it with other Rust items. {:?}",
            e
        )
    });
    let discriminant_field_name = discriminant_field_name
        .map(|n| Ident::new(&n, Span::call_site()))
        .unwrap_or_else(|| {
            format_ident!("{}_type", heck::AsSnekCase(e.ident.to_string()).to_string())
        });
    let discriminant_field_name_js_case = format_ident!(
        "{}",
        heck::AsLowerCamelCase(discriminant_field_name.to_string()).to_string()
    );
    let repr = repr.unwrap_or(DiscriminantRepr::I64);
    let mut discriminant_enum_ident = None;
    let (discriminant_type, discriminant_type_dynamic) = match repr {
        DiscriminantRepr::I64 => (quote!(i64), quote!(i64)),
        DiscriminantRepr::String => (quote!(&'static str), quote!(String)),
        DiscriminantRepr::Enum | DiscriminantRepr::EnumStr => {
            let i = format_ident!("{}Discriminant", e.ident.to_string()).into_token_stream();
            discriminant_enum_ident = Some(i.clone());
            (i.clone(), i)
        }
        DiscriminantRepr::None => {
            let i = quote! {
                compile_error!("type for DiscriminantRepr::None was used, this is a bug in enunion!");
            };
            (i.clone(), i)
        }
    };
    let variants = e
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            VariantComputedData::new(
                &e.ident,
                &discriminant_enum_ident,
                v,
                repr,
                &discriminant_field_name,
                i,
            )
        })
        .collect::<Vec<_>>();
    let struct_variants_iter = || {
        variants.iter().filter_map(|v| match &v.data {
            VariantData::Struct(data) => Some((v.variant, data)),
            _ => None,
        })
    };
    let mod_ident = format_ident!(
        "__enunion_{}",
        heck::AsSnekCase(e.ident.to_string()).to_string()
    );
    let enum_ident = &e.ident;
    let struct_idents = struct_variants_iter()
        .map(|(_v, v_data)| &v_data.struct_ident)
        .collect::<Vec<_>>();
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.
        let ts_path = PathBuf::from("enunion-generated-ts").join(&format!(
            "{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        let js_path = PathBuf::from("enunion-generated-ts").join(&format!(
            "{}ー{}ー{}.js",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        create_dir_all(ts_path.parent().unwrap()).unwrap();
        let mut ts = String::new();
        let mut js = String::new();
        writeln!(
            ts,
            "export type {} = {};",
            enum_ident,
            struct_idents
                .iter()
                .map(|s| s.to_string())
                .chain(
                    variants
                        .iter()
                        .filter_map(|v| {
                            match &v.data {
                                VariantData::Transparent { types } => Some(types),
                                _ => None,
                            }
                        })
                        .map(|types| types
                            .iter()
                            .map(|ty| napi_derive_backend::ty_to_ts_type(ty, false, false).0)
                            .join(" & "))
                )
                .join(" | ")
        )
        .expect("Failed to write to TS output file");
        if repr != DiscriminantRepr::None {
            for (_v, v_data) in struct_variants_iter() {
                writeln!(
                    ts,
                    "export const {ident}: {value};",
                    ident = v_data.const_ident,
                    value = v_data.const_value_ts
                )
                .expect("Failed to write to TS output file");
                match repr {
                    DiscriminantRepr::Enum | DiscriminantRepr::EnumStr => {
                        writeln!(
                            js,
                            "module.exports.{ident} = module.exports.{value};",
                            ident = v_data.const_ident,
                            value = v_data.const_value_ts
                        )
                        .expect("Failed to write to TS output file");
                    }
                    _ => {
                        writeln!(
                            js,
                            "module.exports.{ident} = {value};",
                            ident = v_data.const_ident,
                            value = v_data.const_value_ts
                        )
                        .expect("Failed to write to TS output file");
                    }
                }
            }
        }
        let mut ts_f = File::create(&ts_path).expect("Failed to open TS output file");
        ts_f.write_all(ts.as_bytes()).unwrap();
        let mut js_f = File::create(&js_path).expect("Failed to open JS output file");
        js_f.write_all(js.as_bytes()).unwrap();
    }
    let const_idents = struct_variants_iter()
        .map(|(_v, v_data)| &v_data.const_ident)
        .collect::<Vec<_>>();
    let const_values = struct_variants_iter().map(|(_v, v_data)| &v_data.const_value);
    let ts_type_attrs = struct_variants_iter().map(|(_v, v_data)| {
        let const_value = syn::LitStr::new(&v_data.const_value_ts.to_string(), Span::call_site());
        quote! {
            #[napi(ts_type = #const_value)]
        }
    });
    let struct_fields = struct_variants_iter().map(|(_v, v_data)| {
        let mut pub_fields = v_data.fields.clone();
        for f in &mut pub_fields {
            f.vis = VisPublic {
                pub_token: Pub {
                    span: Span::call_site(),
                },
            }
            .into();
        }
        // The trailing comma is important, because we're adding one more field.
        if !pub_fields.empty_or_trailing() {
            pub_fields.push_punct(Comma {
                spans: [Span::call_site()],
            });
        }
        pub_fields
    });
    let variant_idents = struct_variants_iter()
        .map(|(v, _v_data)| &v.ident)
        .collect::<Vec<_>>();
    let enum_field_idents = struct_variants_iter()
        .map(|(_v, v_data)| {
            v_data
                .fields
                .iter()
                .map(|f| f.ident.as_ref().expect("infallible"))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let enum_field_tokens = struct_variants_iter()
        .map(|(_v, v_data)| &v_data.enum_field_tokens)
        .collect::<Vec<_>>();
    let struct_field_tokens =
        struct_variants_iter().map(|(_v, v_data)| &v_data.struct_field_tokens);
    let js_object_field_tokens = struct_variants_iter()
        .map(|(_v, v_data)| {
            v_data
                .fields
                .iter()
                .map(|f| {
                    format_ident!(
                        "{}",
                        heck::AsLowerCamelCase(f.ident.as_ref().unwrap().to_string()).to_string()
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let ty_compare_expr = match repr {
        DiscriminantRepr::I64 | DiscriminantRepr::Enum | DiscriminantRepr::EnumStr => quote! { ty },
        DiscriminantRepr::String => quote! { ty.as_deref() },
        DiscriminantRepr::None => {
            quote! {
                compile_error!("ty_compare_expr for DiscriminantRepr::None was used, this is a bug in enunion!");
            }
        }
    };
    let enum_pattern_tokens = struct_variants_iter().map(|(_v, v_data)| {
        if v_data.fields.is_empty() {
            proc_macro2::TokenStream::new()
        } else {
            quote! { { .. } }
        }
    });
    let discriminant_enum = discriminant_enum_ident.map(|i| {
        if DiscriminantRepr::EnumStr == repr {
            quote! {
                #[::enunion::string_enum]
                #[derive(Debug, Eq, PartialEq)]
                pub enum #i {
                    #(
                        #variant_idents
                    ),*
                }
            }
        } else {
            quote! {
                #[::napi_derive::napi]
                #[derive(Debug, Eq, PartialEq)]
                pub enum #i {
                    #(
                        #variant_idents
                    ),*
                }
            }
        }
    });
    if repr != DiscriminantRepr::None {
        quote! {
            #e

            #[doc(hidden)]
            mod #mod_ident {
                use ::napi::bindgen_prelude::*;

                impl ::napi::bindgen_prelude::FromNapiValue for super::#enum_ident {
                    unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                        let o = <::napi::JsObject as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)?;
                        let ty: Option<#discriminant_type_dynamic> = o.get(stringify!(#discriminant_field_name_js_case))?;
                        match #ty_compare_expr {
                            #(Some(#const_idents) => Ok(<#struct_idents as Into<super::#enum_ident>>::into(#struct_idents::try_from(o)?)),)*
                            _ => Err(::napi::Error::from_reason(format!("JS object provided was not a valid {}, ty is {:?}", stringify!(#enum_ident), ty)))
                        }
                    }
                }

                impl ::napi::bindgen_prelude::ToNapiValue for super::#enum_ident {
                    unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                        match &val {
                            #(super::#enum_ident::#variant_idents #enum_pattern_tokens => <#struct_idents as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, val.try_into().unwrap()),)*
                        }
                    }
                }

                #discriminant_enum

                #(
                    const #const_idents: #discriminant_type = #const_values;
                    #[::napi_derive::napi(object)]
                    struct #struct_idents {
                        #struct_fields
                        #ts_type_attrs
                        pub #discriminant_field_name: #discriminant_type,
                    }

                    impl From<#struct_idents> for super::#enum_ident {
                        fn from(s: #struct_idents) -> Self {
                            Self::#variant_idents {
                                #(#enum_field_idents: s.#enum_field_idents),*
                            }
                        }
                    }

                    impl TryFrom<super::#enum_ident> for #struct_idents {
                        type Error = ContainedValueIsNotOfThatType;

                        fn try_from(s: super::#enum_ident) -> ::std::result::Result<Self, Self::Error> {
                            match s {
                                super::#enum_ident::#variant_idents #enum_field_tokens => Ok(Self #struct_field_tokens),
                                _ => Err(ContainedValueIsNotOfThatType),
                            }
                        }
                    }

                    impl TryFrom<::napi::JsObject> for #struct_idents {
                        type Error = ::napi::Error;

                        fn try_from(o: ::napi::JsObject) -> ::std::result::Result<Self, Self::Error> {
                            let ty: Option<#discriminant_type_dynamic> = o.get(stringify!(#discriminant_field_name_js_case))?;
                            if #ty_compare_expr != Some(#const_idents) {
                                return Err(::napi::Error::from_reason(format!("provided object was not {}", stringify!(#struct_idents))));
                            }
                            Ok(Self {
                                #(#enum_field_idents: o.get(stringify!(#js_object_field_tokens))?.ok_or_else(|| ::napi::Error::from_reason(format!("conversion to {} failed, field {} is missing", stringify!(#struct_idents), stringify!(#js_object_field_tokens))))?,)*
                                #discriminant_field_name: #const_idents,
                            })
                        }
                    }
                )*

                #[derive(Debug, Copy, Clone)]
                pub struct ContainedValueIsNotOfThatType;
            }
        }
    } else {
        let struct_variants = quote! {
            #(
                #[::napi_derive::napi(object)]
                struct #struct_idents {
                    #struct_fields
                }

                impl From<#struct_idents> for super::#enum_ident {
                    fn from(s: #struct_idents) -> Self {
                        Self::#variant_idents {
                            #(#enum_field_idents: s.#enum_field_idents),*
                        }
                    }
                }

                impl TryFrom<super::#enum_ident> for #struct_idents {
                    type Error = ContainedValueIsNotOfThatType;

                    fn try_from(s: super::#enum_ident) -> ::std::result::Result<Self, Self::Error> {
                        match s {
                            super::#enum_ident::#variant_idents #enum_field_tokens => Ok(Self #enum_field_tokens),
                            _ => Err(ContainedValueIsNotOfThatType),
                        }
                    }
                }

                impl TryFrom<&::napi::JsObject> for #struct_idents {
                    type Error = ::napi::Error;

                    fn try_from(o: &::napi::JsObject) -> ::std::result::Result<Self, Self::Error> {
                        Ok(Self {
                            #(#enum_field_idents: o.get(stringify!(#js_object_field_tokens))?.ok_or_else(|| ::napi::Error::from_reason(format!("conversion to {} failed, field {} is missing", stringify!(#struct_idents), stringify!(#js_object_field_tokens))))?,)*
                        })
                    }
                }
            )*
        };
        let from_attempts = variants.iter()
            .map(|v| {
                match &v.data {
                    VariantData::Struct(s) => {
                        let s_ident = &s.struct_ident;
                        quote! {
                            match <#s_ident as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val) {
                                Ok(v) => {
                                    return Ok(<#s_ident as Into<super::#enum_ident>>::into(v));
                                },
                                Err(e) => {
                                    errs.push(e);
                                },
                            }
                        }
                    },
                    VariantData::Transparent {types} => {
                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();
                        let v_ident = &v.variant.ident;
                        let child_types = types.iter().cloned().map(compute_child_relative_type).collect::<Vec<_>>();
                        quote! {
                            match (
                                #(
                                    <#child_types as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)
                                ),*
                            ) {
                                ( #(Ok(#field_range)),* ) => {
                                    return Ok(super::#enum_ident::#v_ident ( #(#field_range),* ));
                                }
                                ( #(#field_range @ _),* ) => {
                                    #(
                                        if let Err(e) = #field_range {
                                            errs.push(e);
                                        }
                                    )*
                                }
                            }
                        }
                    },
                }
            })
            .collect::<proc_macro2::TokenStream>();
        let into_arms = variants.iter()
            .map(|v| {
                match &v.data {
                    VariantData::Struct(s) => {
                        let s_ident = &s.struct_ident;
                        let v_ident = &v.variant.ident;
                        let enum_pattern_tokens = if s.fields.is_empty() {
                            proc_macro2::TokenStream::new()
                        } else {
                            quote! { { .. } }
                        };
                        quote! {
                            super::#enum_ident::#v_ident #enum_pattern_tokens => <#s_ident as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, val.try_into().unwrap()),
                        }
                    },
                    VariantData::Transparent {types} => {
                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();
                        let v_ident = &v.variant.ident;
                        let child_types = types.iter().cloned().map(compute_child_relative_type).collect::<Vec<_>>();
                        let compute_value = if child_types.len() == 1 {
                            let t = &child_types[0];
                            quote! {
                                <#t as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, _0)
                            }
                        } else {
                            // This usage only really makes sense with objects. Otherwise it's not
                            // possible to merge these.
                            quote! {
                                {
                                    let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
                                    let mut merged_object = env.create_object().unwrap();
                                    #(
                                        let sub_object = unsafe { <::napi::JsObject as ::napi::NapiValue>::from_raw(__enunion_env, <#child_types as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, #field_range).unwrap()).expect("enunion doesn't support intersection types where the members aren't objects") };
                                        let keys = ::napi::JsObject::keys(&sub_object).unwrap();
                                        for key in keys {
                                            merged_object.set_named_property::<::napi::JsUnknown>(&key, sub_object.get_named_property::<::napi::JsUnknown>(&key).unwrap()).unwrap();
                                        }
                                    )*
                                    Ok(<::napi::JsObject as ::napi::NapiRaw>::raw(&merged_object))
                                }
                            }
                        };
                        quote! {
                            super::#enum_ident::#v_ident(#(#field_range),*) => #compute_value,
                        }
                    },
                }
            })
            .collect::<proc_macro2::TokenStream>();
        quote! {
            #e

            #[doc(hidden)]
            mod #mod_ident {
                use ::napi::bindgen_prelude::*;

                impl ::napi::bindgen_prelude::FromNapiValue for super::#enum_ident {
                    unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                        let mut errs = Vec::new();
                        #from_attempts
                        Err(::napi::Error::from_reason(format!("JS object provided was not a valid {}, no variants deserialized correctly. Errors: {:?}", stringify!(#enum_ident), errs)))
                    }
                }

                impl ::napi::bindgen_prelude::ToNapiValue for super::#enum_ident {
                    unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                        match val {
                            #into_arms
                        }
                    }
                }

                #struct_variants

                #[derive(Debug, Copy, Clone)]
                pub struct ContainedValueIsNotOfThatType;
            }
        }
    }.into()
}

fn compute_child_relative_type(mut ty: syn::Type) -> syn::Type {
    match &mut ty {
        syn::Type::Path(syn::TypePath { path, .. }) => {
            if path.leading_colon.is_none() {
                let compute_super = || PathSegment {
                    ident: Ident::new("super", Span::call_site()),
                    arguments: PathArguments::None,
                };
                match path.segments[0].ident.to_string().as_str() {
                    "self" => {
                        path.segments[0] = compute_super();
                    }
                    "crate" | "Self" => {
                        // Do nothing, this path is already absolute.
                    }
                    _ => {
                        path.segments.insert(0, compute_super());
                    }
                }
            }
        }
        _ => {
            // Do nothing, this is already absolute.
        }
    }
    ty
}

enum VariantData {
    Struct(VariantStructData),
    Transparent { types: Vec<syn::Type> },
}

struct VariantComputedData<'a> {
    variant: &'a Variant,
    data: VariantData,
}

struct VariantStructData {
    fields: Punctuated<Field, Comma>,
    const_ident: Ident,
    struct_ident: Ident,
    const_value: proc_macro2::TokenStream,
    const_value_ts: String,
    enum_field_tokens: proc_macro2::TokenStream,
    struct_field_tokens: proc_macro2::TokenStream,
}

impl<'a> VariantComputedData<'a> {
    pub fn new(
        enum_ident: &Ident,
        discriminant_enum_ident: &Option<proc_macro2::TokenStream>,
        variant: &'a Variant,
        repr: DiscriminantRepr,
        discriminant_field_name: &Ident,
        variant_index: usize,
    ) -> Self {
        if variant.discriminant.is_some() {
            abort_call_site!("Manually specified discriminants are not supported, please remove the discriminant on {}::{}", enum_ident, variant.ident);
        }
        let fields = match &variant.fields {
            Fields::Named(named) => named.named.clone(),
            Fields::Unit => Punctuated::new(),
            Fields::Unnamed(unnamed) => {
                if repr != DiscriminantRepr::None {
                    abort_call_site!("Using a discriminant is not compatible with tuple enum variants such as {}::{}, try using named fields instead. Alternatively, use \"none\" discriminant_repr.", enum_ident, variant.ident)
                }
                return Self {
                    variant,
                    data: VariantData::Transparent {
                        types: unnamed.unnamed.iter().map(|f| f.ty.clone()).collect(),
                    },
                };
            }
        };
        let const_ident = format_ident!(
            "{}_TYPE_{}",
            heck::AsShoutySnekCase(enum_ident.to_string()).to_string(),
            heck::AsShoutySnekCase(variant.ident.to_string()).to_string()
        );
        let (const_value, const_value_ts) = match repr {
            DiscriminantRepr::I64 => {
                let i: i64 = variant_index.try_into().expect("too many variants!");
                (quote! { #i }, i.to_string())
            }
            DiscriminantRepr::Enum | DiscriminantRepr::EnumStr => {
                let v = &variant.ident;
                let discriminant_enum_ident = discriminant_enum_ident.as_ref().unwrap();
                (
                    quote! { #discriminant_enum_ident::#v },
                    format!("{}.{}", discriminant_enum_ident, v),
                )
            }
            DiscriminantRepr::String => (
                quote! { stringify!(#const_ident) },
                format!("\"{}\"", const_ident),
            ),
            DiscriminantRepr::None => (
                quote! {
                    compile_error!("const_value for DiscriminantRepr::None was used, this is a bug in enunion!");
                },
                String::from(
                    "const_value_ts for DiscriminantRepr::None was used, this is a bug in enunion!",
                ),
            ),
        };
        let mut enum_field_tokens = fields
            .iter()
            .map(|f| {
                let ident = &f.ident;
                quote! {
                    #ident,
                }
            })
            .collect::<proc_macro2::TokenStream>();
        let mut struct_field_tokens = enum_field_tokens.clone();
        struct_field_tokens.extend(quote! {
            #discriminant_field_name: #const_ident,
        });
        for t in &mut [&mut struct_field_tokens, &mut enum_field_tokens] {
            let mut ret = proc_macro2::TokenStream::new();
            let t_clone = t.clone();
            Brace {
                span: Span::call_site(),
            }
            .surround(&mut ret, |ret| ret.extend(t_clone));
            **t = ret;
        }
        let struct_ident = format_ident!("{}{}", enum_ident, variant.ident);
        let data = VariantStructData {
            fields,
            struct_ident,
            const_ident,
            const_value,
            const_value_ts,
            enum_field_tokens,
            struct_field_tokens,
        };
        Self {
            variant,
            data: VariantData::Struct(data),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum DiscriminantRepr {
    I64,
    String,
    Enum,
    EnumStr,
    None,
}

struct Args {
    items: Punctuated<MetaNameValue, Comma>,
}

impl Parse for Args {
    fn parse(input: ParseStream<'_>) -> syn::parse::Result<Self> {
        Punctuated::<_, _>::parse_terminated(input).map(|items| Args { items })
    }
}

#[proc_macro_error::proc_macro_error]
#[proc_macro_attribute]
pub fn string_enum(_attr_input: TokenStream, item: TokenStream) -> TokenStream {
    let mut e: ItemEnum = syn::parse(item).unwrap_or_else(|e| {
        abort_call_site!(
            "string_enum only supports enums, do not use it with other Rust items. {:?}",
            e
        )
    });
    let str_literals = e
        .variants
        .iter()
        .map(|v| {
            v.discriminant
                .as_ref()
                .map(|(_eq, d)| match d {
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) => s.value(),
                    _ => {
                        abort_call_site!("string_enum only supports string literal discriminants!")
                    }
                })
                .unwrap_or_else(|| v.ident.to_string())
        })
        .collect::<Vec<_>>();
    let enum_ident = &e.ident;
    let variant_idents = e
        .variants
        .iter()
        .map(|v| v.ident.clone())
        .collect::<Vec<_>>();
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.

        // Add some leading underscores so these will sort to the top (important for JS code)
        let ts_path = PathBuf::from("enunion-generated-ts").join(&format!(
            "_{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));

        let js_path = PathBuf::from("enunion-generated-ts").join(&format!(
            "_{}ー{}ー{}.js",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        create_dir_all(ts_path.parent().unwrap()).unwrap();
        let mut ts = String::new();
        let mut js = String::new();
        writeln!(ts, "export const enum {} {{", enum_ident)
            .expect("Failed to write to TS output file");
        writeln!(js, "module.exports.{} = {{", enum_ident)
            .expect("Failed to write to TS output file");
        for (i, v) in e.variants.iter().enumerate() {
            writeln!(
                ts,
                "  {ident} = \"{value}\",",
                ident = v.ident,
                value = str_literals[i]
            )
            .expect("Failed to write to TS output file");
            writeln!(
                js,
                "  {ident}: \"{value}\",",
                ident = v.ident,
                value = str_literals[i]
            )
            .expect("Failed to write to JS output file");
        }
        writeln!(js, "}};").expect("Failed to write to JS output file");
        writeln!(ts, "}}").expect("Failed to write to TS output file");
        let mut ts_f = File::create(&ts_path).expect("Failed to open TS output file");
        ts_f.write_all(ts.as_bytes()).unwrap();
        let mut js_f = File::create(&js_path).expect("Failed to open JS output file");
        js_f.write_all(js.as_bytes()).unwrap();
    }
    for v in &mut e.variants {
        v.discriminant = None;
    }
    quote! {
        #e

        impl ::napi::bindgen_prelude::FromNapiValue for #enum_ident {
            unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                let v = <String as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)?;
                match v.as_str() {
                    #(#str_literals => Ok(#enum_ident::#variant_idents),)*
                    _ => Err(::napi::Error::from_reason(format!("JS string provided was not a valid {}, string is {:?}", stringify!(#enum_ident), v)))
                }
            }
        }

        impl ::napi::bindgen_prelude::ToNapiValue for #enum_ident {
            unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                match &val {
                    #(#enum_ident::#variant_idents => ::napi::bindgen_prelude::ToNapiValue::to_napi_value(__enunion_env, #str_literals),)*
                }
            }
        }

        impl ::std::fmt::Display for #enum_ident {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::result::Result<(), ::std::fmt::Error> {
                match self {
                    #(#enum_ident::#variant_idents => write!(f, "{}", #str_literals),)*
                }
            }
        }

        impl ::std::str::FromStr for #enum_ident {
            type Err = String;
            fn from_str(s: &str) -> ::std::result::Result<Self, Self::Err> {
                match s {
                    #(#str_literals => Ok(#enum_ident::#variant_idents),)*
                    _ => Err(format!("string provided was not a valid {}, string is {:?}", stringify!(#enum_ident), s))
                }
            }
        }
    }.into()
}
