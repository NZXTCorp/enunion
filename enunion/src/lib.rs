use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_error::{abort, abort_call_site};
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
    Expr, ExprLit, Field, Fields, Ident, ItemEnum, Lit, LitStr, MetaNameValue, Variant, VisPublic,
};

const SUPPORTED_REPR_TYPES: &str =
    "\"i64\", \"enum\", \"enum_str\", \"none\", \"str\", or \"bool\"";

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
/// - "bool" - Only two variants are permitted, the first will be false, the second will be true.
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
                        "bool" => {
                            repr = Some(DiscriminantRepr::Bool);
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
                            abort_call_site!("{} is not a recognized representation, please use {}", other, SUPPORTED_REPR_TYPES)
                        }
                    }
                }
                _ => abort_call_site!("only string literals are supported for the discriminant_repr, please provide {}", SUPPORTED_REPR_TYPES)
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
    if let Some(lt) = &e.generics.lt_token {
        abort!(
            lt.spans[0],
            "enunion does not support generics. Please remove the {} in this declaration.",
            e.generics.into_token_stream()
        );
    }
    let discriminant_field_name = discriminant_field_name
        .map(|n| Ident::new(&n, Span::call_site()))
        .unwrap_or_else(|| {
            format_ident!("{}_type", heck::AsSnekCase(e.ident.to_string()).to_string())
        });
    let discriminant_field_name_js_case = LitStr::new(
        &heck::AsLowerCamelCase(discriminant_field_name.to_string()).to_string(),
        Span::call_site(),
    );
    let repr = repr.unwrap_or(DiscriminantRepr::I64);
    let mut discriminant_enum_ident = None;
    let (discriminant_type, discriminant_type_dynamic) = match repr {
        DiscriminantRepr::I64 => (quote!(i64), quote!(i64)),
        DiscriminantRepr::Bool => (quote!(bool), quote!(bool)),
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
            VariantData::Struct(data) => Some((v, data)),
            _ => None,
        })
    };
    if repr != DiscriminantRepr::None {
        // Check for duplicate discriminant values
        let mut sorted_idents = variants
            .iter()
            .map(|v| (&v.variant.ident, &v.const_value))
            .collect::<Vec<_>>();
        sorted_idents.sort_unstable_by_key(|(_variant_ident, value)| value.to_string());
        for w in sorted_idents.windows(2) {
            if w[0].1.to_string() == w[1].1.to_string() {
                abort_call_site!("`{}` contains duplicate discriminants, `{}` and `{}` would collide. Change one of the discriminant values to an unused value.", e.ident.to_string(), w[0].0.to_string(), w[1].0.to_string());
            }
        }
    }
    let mod_ident = format_ident!(
        "__enunion_{}",
        heck::AsSnekCase(e.ident.to_string()).to_string()
    );
    let init_fn_idents = variants.iter().map(|v| {
        format_ident!(
            "____napi_register__enunion_{}",
            heck::AsSnekCase(v.variant.ident.to_string()).to_string()
        )
    });
    let cb_names = variants.iter().map(|v| {
        format_ident!(
            "__enunion_callback_{}",
            heck::AsSnekCase(v.variant.ident.to_string()).to_string()
        )
    });
    let enum_ident = &e.ident;
    let struct_idents = || struct_variants_iter().map(|(_v, v_data)| &v_data.struct_ident);
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.
        let ts_path = gen_ts_folder().join(&format!(
            "{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        let js_path = gen_ts_folder().join(&format!(
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
            (struct_idents)()
                .map(|s| s.to_string())
                .chain(
                    variants
                        .iter()
                        .filter_map(|v| {
                            match &v.data {
                                VariantData::Transparent { types } => {
                                    Some((types, &v.const_value_ts))
                                }
                                _ => None,
                            }
                        })
                        .map(|(types, const_value_ts)| types
                            .iter()
                            .map(|ty| napi_derive_backend::ty_to_ts_type(ty, false, false).0)
                            .chain((repr != DiscriminantRepr::None).then(|| format!(
                                "{{ {}: {} }}",
                                discriminant_field_name_js_case.value(),
                                const_value_ts
                            )))
                            .join(" & "))
                )
                .join(" | ")
        )
        .expect("Failed to write to TS output file");
        if repr != DiscriminantRepr::None {
            for v in variants.iter() {
                writeln!(
                    ts,
                    "export const {ident}: {value};",
                    ident = v.const_ident,
                    value = v.const_value_ts
                )
                .expect("Failed to write to TS output file");
                writeln!(
                    js,
                    "module.exports.{ident} = nativeBinding.{ident};",
                    ident = v.const_ident,
                )
                .expect("Failed to write to JS output file");
            }
        }
        let mut ts_f = File::create(&ts_path).expect("Failed to open TS output file");
        ts_f.write_all(ts.as_bytes()).unwrap();
        let mut js_f = File::create(&js_path).expect("Failed to open JS output file");
        js_f.write_all(js.as_bytes()).unwrap();
    }
    let const_idents = variants.iter().map(|v| &v.const_ident);
    let struct_const_idents = struct_variants_iter().map(|(v, _v_data)| &v.const_ident);
    let const_values = variants.iter().map(|v| &v.const_value);
    let ts_type_attrs = struct_variants_iter().map(|(v, _v_data)| {
        let const_value = syn::LitStr::new(&v.const_value_ts.to_string(), Span::call_site());
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
    let variant_idents = variants.iter().map(|v| &v.variant.ident);
    let struct_variant_idents = struct_variants_iter().map(|(v, _v_data)| &v.variant.ident);
    let enum_field_idents = struct_variants_iter().map(|(_v, v_data)| {
        v_data
            .fields
            .iter()
            .map(|f| f.ident.as_ref().expect("infallible"))
            .collect::<Vec<_>>()
    });
    let enum_field_tokens = struct_variants_iter().map(|(_v, v_data)| &v_data.enum_field_tokens);
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
        DiscriminantRepr::I64
        | DiscriminantRepr::Enum
        | DiscriminantRepr::EnumStr
        | DiscriminantRepr::Bool => quote! { ty },
        DiscriminantRepr::String => quote! { ty.as_deref() },
        DiscriminantRepr::None => {
            quote! {
                compile_error!("ty_compare_expr for DiscriminantRepr::None was used, this is a bug in enunion!");
            }
        }
    };
    let discriminant_enum_idents = variants.iter().map(|v| {
        v.discriminant_value
            .as_ref()
            .map(|v| Ident::new(v, Span::call_site()))
            .unwrap_or_else(|| v.variant.ident.clone())
    });

    let discriminant_enum = discriminant_enum_ident.map(|i| {
        if DiscriminantRepr::EnumStr == repr {
            quote! {
                #[::enunion::string_enum]
                #[derive(Debug, Eq, PartialEq)]
                pub enum #i {
                    #(
                        #discriminant_enum_idents
                    ),*
                }
            }
        } else {
            quote! {
                #[::napi_derive::napi]
                #[derive(Debug, Eq, PartialEq)]
                pub enum #i {
                    #(
                        #discriminant_enum_idents
                    ),*
                }
            }
        }
    });
    let mut e_altered = e.clone();
    // remove the `enunion` attributes from the resulting enum, they've been parsed.
    for v in &mut e_altered.variants {
        v.attrs.retain(|a| {
            a.path
                .segments
                .last()
                .map(|i| i.ident.to_string())
                .as_deref()
                != Some("enunion")
        });
    }
    let struct_idents = (struct_idents)();
    if repr != DiscriminantRepr::None {
        let from_arms = variants.iter()
            .map(|v| {
                let v_ident = &v.variant.ident;
                let const_ident = &v.const_ident;
                match &v.data {
                    VariantData::Struct(data) => {
                        let struct_ident = &data.struct_ident;
                        quote! {
                            Some(#const_ident) => Ok(<#struct_ident as Into<super::#enum_ident>>::into(#struct_ident::try_from(o)?)),
                        }
                    }
                    VariantData::Transparent { types} => {
                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();
                        quote! {
                            Some(#const_ident) => match (
                                #(
                                    <#types as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)
                                ),*
                            ) {
                                ( #(Ok(#field_range)),* ) => {
                                    Ok(super::#enum_ident::#v_ident ( #(#field_range),* ))
                                }
                                ( #(#field_range),* ) => {
                                    let mut errs = Vec::new();
                                    #(
                                        if let Err(e) = #field_range {
                                            errs.push(e);
                                        }
                                    )*
                                    Err(::napi::Error::from_reason(format!("JS object provided was not a valid {}, discriminant is {:?}, but encountered errors deserializing as that type {:?}", stringify!(#enum_ident), ty, errs)))
                                }
                            }
                        }
                    }
                }
            })
            .collect::<proc_macro2::TokenStream>();
        let into_arms = variants.iter()
            .map(|v| {
                let variant_ident = &v.variant.ident;
                match &v.data {
                    VariantData::Struct(data) => {
                        let enum_pattern_tokens = if data.fields.is_empty() {
                            proc_macro2::TokenStream::new()
                        } else {
                            quote! { { .. } }
                        };
                        let struct_ident = &data.struct_ident;
                        quote! {
                            val @ super::#enum_ident::#variant_ident #enum_pattern_tokens => <#struct_ident as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, val.try_into().unwrap()),
                        }
                    }
                    VariantData::Transparent { types} => {
                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();
                        let const_ident = &v.const_ident;
                        quote! {
                            super::#enum_ident::#variant_ident(#(#field_range),*) => {
                                let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
                                let mut merged_object = env.create_object()?;
                                #(
                                    {
                                        let sub_object = unsafe { <::napi::JsObject as ::napi::NapiValue>::from_raw(__enunion_env, <#types as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, #field_range)?).expect("enunion doesn't support intersection types where the members aren't objects") };
                                        let keys = ::napi::JsObject::keys(&sub_object)?;
                                        for key in keys {
                                            merged_object.set_named_property::<::napi::JsUnknown>(&key, sub_object.get_named_property::<::napi::JsUnknown>(&key)?)?;
                                        }
                                    }
                                )*
                                merged_object.set(#discriminant_field_name_js_case, #const_ident)?;
                                Ok(<::napi::JsObject as ::napi::NapiRaw>::raw(&merged_object))
                            },
                        }
                    }
                }
            })
            .collect::<proc_macro2::TokenStream>();
        quote! {
            #e_altered

            #[doc(hidden)]
            mod #mod_ident {
                use ::napi::bindgen_prelude::*;
                use super::*;

                impl ::napi::bindgen_prelude::FromNapiValue for super::#enum_ident {
                    unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                        let o = <::napi::JsObject as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)?;
                        let ty: Option<#discriminant_type_dynamic> = o.get(#discriminant_field_name_js_case)?;
                        match #ty_compare_expr {
                            #from_arms
                            _ => Err(::napi::Error::from_reason(format!("JS object provided was not a valid {}, ty is {:?}", stringify!(#enum_ident), ty)))
                        }
                    }
                }

                impl ::napi::bindgen_prelude::ToNapiValue for super::#enum_ident {
                    unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                        match val {
                            #into_arms
                        }
                    }
                }

                #discriminant_enum

                #(
                    const #const_idents: #discriminant_type = #const_values;

                    #[allow(non_snake_case)]
                    #[allow(clippy::all)]
                    unsafe fn #cb_names(env: napi::sys::napi_env) -> napi::Result<napi::sys::napi_value> {
                        <#discriminant_type as napi::bindgen_prelude::ToNapiValue>::to_napi_value(env, #const_idents)
                    }
                    #[allow(non_snake_case)]
                    #[allow(clippy::all)]
                    #[cfg(not(test))]
                    #[::napi::bindgen_prelude::ctor]
                    fn #init_fn_idents() {
                        ::napi::bindgen_prelude::register_module_export(None, concat!(stringify!(#const_idents), "\0"), #cb_names);
                    }
                )*

                #(
                    #[::napi_derive::napi(object)]
                    struct #struct_idents {
                        #struct_fields
                        #ts_type_attrs
                        pub #discriminant_field_name: #discriminant_type,
                    }

                    impl From<#struct_idents> for super::#enum_ident {
                        fn from(s: #struct_idents) -> Self {
                            Self::#struct_variant_idents {
                                #(#enum_field_idents: s.#enum_field_idents),*
                            }
                        }
                    }

                    impl TryFrom<super::#enum_ident> for #struct_idents {
                        type Error = ContainedValueIsNotOfThatType;

                        fn try_from(s: super::#enum_ident) -> ::std::result::Result<Self, Self::Error> {
                            match s {
                                super::#enum_ident::#struct_variant_idents #enum_field_tokens => Ok(Self #struct_field_tokens),
                                _ => Err(ContainedValueIsNotOfThatType),
                            }
                        }
                    }

                    impl TryFrom<::napi::JsObject> for #struct_idents {
                        type Error = ::napi::Error;

                        fn try_from(o: ::napi::JsObject) -> ::std::result::Result<Self, Self::Error> {
                            let ty: Option<#discriminant_type_dynamic> = o.get(#discriminant_field_name_js_case)?;
                            if #ty_compare_expr != Some(#struct_const_idents) {
                                return Err(::napi::Error::from_reason(format!("provided object was not {}", stringify!(#struct_idents))));
                            }
                            Ok(Self {
                                #(#enum_field_idents: o.get(stringify!(#js_object_field_tokens))?.ok_or_else(|| ::napi::Error::from_reason(format!("conversion to {} failed, field {} is missing", stringify!(#struct_idents), stringify!(#js_object_field_tokens))))?,)*
                                #discriminant_field_name: #struct_const_idents,
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
                        quote! {
                            // This is a loop that's guaranteed to execute exactly once.
                            // Mostly I just wanted to be able to use the `break` keyword.
                            for _ in Some(()) {
                                #(
                                    let #field_range = match <#types as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val) {
                                        Ok(#field_range) => #field_range,
                                        Err(e) => {
                                            errs.push(e);
                                            break;
                                        }
                                    };
                                )*
                                return Ok(super::#enum_ident::#v_ident ( #(#field_range),* ));
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
                        let compute_value = if types.len() == 1 {
                            let t = &types[0];
                            quote! {
                                <#t as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, _0)
                            }
                        } else {
                            // This usage only really makes sense with objects. Otherwise it's not
                            // possible to merge these.
                            quote! {
                                {
                                    let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
                                    let mut merged_object = env.create_object()?;
                                    #(
                                        {
                                            let sub_object = unsafe { <::napi::JsObject as ::napi::NapiValue>::from_raw(__enunion_env, <#types as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, #field_range)?).expect("enunion doesn't support intersection types where the members aren't objects") };
                                            let keys = ::napi::JsObject::keys(&sub_object)?;
                                            for key in keys {
                                                merged_object.set_named_property::<::napi::JsUnknown>(&key, sub_object.get_named_property::<::napi::JsUnknown>(&key)?)?;
                                            }
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
            #e_altered

            #[doc(hidden)]
            mod #mod_ident {
                use ::napi::bindgen_prelude::*;
                use super::*;

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

#[allow(clippy::large_enum_variant)] // Large variant is most common
enum VariantData {
    Struct(VariantStructData),
    Transparent { types: Vec<syn::Type> },
}

struct VariantComputedData<'a> {
    variant: &'a Variant,
    data: VariantData,
    const_ident: Ident,
    const_value: proc_macro2::TokenStream,
    const_value_ts: String,
    discriminant_value: Option<String>,
}

struct VariantStructData {
    fields: Punctuated<Field, Comma>,
    struct_ident: Ident,
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
            abort_call_site!("Please use #[enunion(discriminant_value = \"value\") to specify the discriminant value, the ` = value` style on {}::{} is not supported", enum_ident, variant.ident);
        }
        let const_ident = format_ident!(
            "{}_TYPE_{}",
            heck::AsShoutySnekCase(enum_ident.to_string()).to_string(),
            heck::AsShoutySnekCase(variant.ident.to_string()).to_string()
        );
        let variant_args = variant
            .attrs
            .iter()
            .find(|a| a
                .path
                .get_ident()
                .map(ToString::to_string)
                .as_deref() == Some("enunion")
            )
            .map(|a| a.parse_args::<Args>().unwrap_or_else(|e| abort_call_site!("Failed to parse enunion variant `{}` attribute input, please use field = \"value\" in a comma separated list. Check the documentation for examples. Error: {:?}", variant.ident, e)));
        let mut discriminant_value = None;
        if let Some(variant_args) = variant_args {
            for arg in variant_args.items {
                match arg.path.get_ident().map(|i| i.to_string()).as_deref() {
                    Some("discriminant_value") => match arg.lit {
                        Lit::Str(s) => {
                            discriminant_value = Some(s.value());
                        }
                        Lit::Int(i) => {
                            discriminant_value = Some(i.base10_digits().to_string());
                        }
                        _ => {
                            abort_call_site!("literal type provided for {} is not supported, please use a string or an integer.", variant.ident);
                        }
                    }
                    _ => abort_call_site!("{} was not a recognized enunion argument, please use `discriminant_value`.", arg.path.to_token_stream())
                }
            }
        }
        let (const_value, const_value_ts) = match repr {
            DiscriminantRepr::I64 => {
                let i: i64 = discriminant_value
                    .as_deref()
                    .map(|s| s
                        .parse()
                        .unwrap_or_else(|e| abort_call_site!("discriminant_repr is i64, but discriminant_value is not an i64, this is not supported. {:?}", e)))
                    .unwrap_or_else(|| variant_index.try_into().expect("too many variants!"));
                (quote! { #i }, i.to_string())
            }
            DiscriminantRepr::Bool => {
                let b: bool = discriminant_value
                    .as_deref()
                    .map(|s| s
                        .parse()
                        .unwrap_or_else(|e| abort_call_site!("discriminant_repr is bool, but discriminant_value is not a bool, this is not supported. {:?}", e)))
                    .unwrap_or_else(|| match variant_index {
                        0 => Some(false),
                        1 => Some(true),
                        _ => None,
                    }.unwrap_or_else(|| abort_call_site!("only two variants are supported with discriminant_repr = \"bool\"")));
                (quote! { #b }, b.to_string())
            }
            DiscriminantRepr::Enum | DiscriminantRepr::EnumStr => {
                let v = discriminant_value
                    .as_ref()
                    .map(|s| Ident::new(s, Span::call_site()))
                    .unwrap_or_else(|| variant.ident.clone());
                let discriminant_enum_ident = discriminant_enum_ident.as_ref().unwrap();
                (
                    quote! { #discriminant_enum_ident::#v },
                    format!("{}.{}", discriminant_enum_ident, v),
                )
            }
            DiscriminantRepr::String => {
                let value = discriminant_value
                    .as_ref()
                    .map(|s| Ident::new(s, Span::call_site()))
                    .unwrap_or_else(|| variant.ident.clone());
                (quote! { stringify!(#value) }, format!("\"{}\"", value))
            }
            DiscriminantRepr::None => (
                quote! {
                    compile_error!("const_value for DiscriminantRepr::None was used, this is a bug in enunion!");
                },
                String::from(
                    "const_value_ts for DiscriminantRepr::None was used, this is a bug in enunion!",
                ),
            ),
        };
        let compute_struct_variant = |fields: Punctuated<Field, Comma>| {
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
            VariantData::Struct(VariantStructData {
                fields,
                struct_ident,
                enum_field_tokens,
                struct_field_tokens,
            })
        };
        let data = match &variant.fields {
            Fields::Named(named) => (compute_struct_variant)(named.named.clone()),
            Fields::Unit => {
                if DiscriminantRepr::None == repr {
                    abort!(variant.ident.span(), "discriminant_repr = \"none\" cannot be used with a unit variant. You must add fields.")
                }
                (compute_struct_variant)(Punctuated::new())
            }
            Fields::Unnamed(unnamed) => VariantData::Transparent {
                types: unnamed.unnamed.iter().map(|f| f.ty.clone()).collect(),
            },
        };
        Self {
            variant,
            const_ident,
            const_value,
            const_value_ts,
            discriminant_value,
            data,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum DiscriminantRepr {
    I64,
    Bool,
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
    let e: ItemEnum = syn::parse(item).unwrap_or_else(|e| {
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
    let variant_idents = e.variants.iter().map(|v| &v.ident).collect::<Vec<_>>();
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.

        // Add some leading underscores so these will sort to the top (important for JS code)
        let ts_path = gen_ts_folder().join(&format!(
            "_{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));

        let js_path = gen_ts_folder().join(&format!(
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
        writeln!(
            js,
            "module.exports.{ident} = nativeBinding.{ident}",
            ident = enum_ident
        )
        .expect("Failed to write to TS output file");
        for (i, v) in e.variants.iter().enumerate() {
            writeln!(
                ts,
                "  {ident} = \"{value}\",",
                ident = v.ident,
                value = str_literals[i]
            )
            .expect("Failed to write to TS output file");
        }
        writeln!(ts, "}}").expect("Failed to write to TS output file");
        let mut ts_f = File::create(&ts_path).expect("Failed to open TS output file");
        ts_f.write_all(ts.as_bytes()).unwrap();
        let mut js_f = File::create(&js_path).expect("Failed to open JS output file");
        js_f.write_all(js.as_bytes()).unwrap();
    }
    let mut altered_e = e.clone();
    for v in &mut altered_e.variants {
        v.discriminant = None;
    }
    let init_fn_ident = format_ident!(
        "____napi_register__enunion_{}",
        heck::AsSnekCase(enum_ident.to_string()).to_string()
    );
    let cb_name = format_ident!(
        "__enunion_callback_{}",
        heck::AsSnekCase(enum_ident.to_string()).to_string()
    );
    quote! {
        #altered_e

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
                write!(f, "{}", <Self as ::std::convert::AsRef<str>>::as_ref(self))
            }
        }

        impl ::std::convert::AsRef<str> for #enum_ident {
            fn as_ref(&self) -> &str {
                match self {
                    #(#enum_ident::#variant_idents => #str_literals,)*
                }
            }
        }

        impl ::std::convert::From<#enum_ident> for &'static str {
            fn from(e: #enum_ident) -> Self {
                match e {
                    #(#enum_ident::#variant_idents => #str_literals,)*
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

        #[doc(hidden)]
        #[allow(non_snake_case)]
        #[allow(clippy::all)]
        unsafe fn #cb_name(__enunion_env: napi::sys::napi_env) -> napi::Result<napi::sys::napi_value> {
            let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
            let mut enum_object = env.create_object()?;
            #(
                enum_object.set(#str_literals, #str_literals)?;
            )*
             Ok(<::napi::JsObject as ::napi::NapiRaw>::raw(&enum_object))
        }
        #[doc(hidden)]
        #[allow(non_snake_case)]
        #[allow(clippy::all)]
        #[cfg(not(test))]
        #[::napi::bindgen_prelude::ctor]
        fn #init_fn_ident() {
            ::napi::bindgen_prelude::register_module_export(None, concat!(stringify!(#enum_ident), "\0"), #cb_name);
        }
    }.into()
}

fn gen_ts_folder() -> PathBuf {
    PathBuf::from(var("CARGO_MANIFEST_DIR").unwrap()).join("enunion-generated-ts")
}
