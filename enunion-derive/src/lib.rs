use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_error::{abort, abort_call_site, emit_error};
use quote::{format_ident, quote, ToTokens};
use std::borrow::Cow;
use std::env::var;
use std::fmt::Write as _;
use std::fs::{create_dir_all, File};
use std::io::Write;
use std::path::PathBuf;
use syn::token::And;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Comma, Pub},
    Attribute, Expr, ExprCall, ExprLit, ExprPath, Field, Fields, Ident, ItemEnum, Lifetime, Lit,
    LitStr, Meta, MetaNameValue, Path, PathArguments, PathSegment, Token, Type, TypePath,
    TypeReference, Variant,
};

use convert_case::{Case, Casing};
use sha2::Digest;
use syn::Visibility;

type ProcMacroErrors = Vec<(Span, String)>;

const SUPPORTED_REPR_TYPES: &str =
    "\"i64\", \"enum\", \"enum_str\", \"none\", \"str\", or \"bool\"";

/// This macro is applied to Rust enums. It generates code that will expose the enum to TypeScript as a discriminated union. It uses `napi` to accomplish this.
/// Enunion also handles automatically converting between the two representations, in Rust you can define `#[napi]` methods that accept the enum as an argument, or return an instance of that enum.
/// The TypeScript will be free to handle it as a discriminated union, the Rust can handle it as an enum, and enunion will take care of translating at the boundary.
///
/// **This macro will not work if `napi` and `napi_derive` are not specified in the `[dependencies]` section of the `Cargo.toml`.**
///
/// This macro has a companion program called `enapi` which is a mostly transparent wrapper around the `napi` CLI. You must
/// use `enapi` instead of `napi` for your build. Otherwise the resulting TypeScript will not compile.
/// Here's an example of what that might look like in your package.json file.
///
/// ```json
/// "scripts": {
///   "build": "enapi build --platform --release",
/// },
/// ```
///
/// To install `enapi` execute this command inside a clone of this repository
///
/// ```ignore
/// cargo install --path ./enapi
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
    let mut export_variant_types = None;
    for MetaNameValue { path, value, .. } in attr.items.iter() {
        match path.get_ident().map(|i| i.to_string()).as_deref() {
            Some("discriminant_repr") => match value {
                Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) => {
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
                match value {
                    Expr::Lit(ExprLit { lit: Lit::Str(s), .. })=> {
                        discriminant_field_name = Some(s.value());
                    }
                    _ => abort_call_site!("only string literals are supported for the discriminant_field_name, please provide a string")
                },
            Some("export_variant_types") => match value {
                Expr::Lit(ExprLit { lit: Lit::Bool(b), .. }) => {
                    export_variant_types = Some(b.value());
                },
                _ => abort_call_site!("only bool literals are supported for export_variant_types, please provide a bool")
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
        .unwrap_or_else(|| format!("{}_type", e.ident.to_string().to_case(Case::Snake)));
    let discriminant_field_name_js_case = LitStr::new(
        &discriminant_field_name.to_case(Case::Camel),
        Span::call_site(),
    );
    let discriminant_field_name = format_ident!("r#{}", discriminant_field_name);
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
    let variants = {
        let mut errs = Vec::new();
        let mut variants = Vec::new();
        let iter = e
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
            .map(|r| match r {
                Ok(v) => variants.push(v),
                Err(e) => errs.push(e),
            });
        for _ in iter {}
        if !errs.is_empty() {
            let mut errs = errs.into_iter().peekable();
            while let Some(err) = errs.next() {
                if errs.peek().is_some() {
                    emit_error!(err.0, "{}", err.1);
                } else {
                    abort!(err.0, "{}", err.1);
                }
            }
            unreachable!()
        } else {
            variants
        }
    };
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
    let mod_ident = format_ident!("__enunion_{}", e.ident.to_string().to_case(Case::Snake));
    let init_fn_idents = variants.iter().map(|v| {
        format_ident!(
            "____napi_register__enunion_{}",
            v.variant.ident.to_string().to_case(Case::Snake)
        )
    });
    let cb_names = variants.iter().map(|v| {
        format_ident!(
            "__enunion_callback_{}",
            v.variant.ident.to_string().to_case(Case::Snake)
        )
    });
    let enum_ident = &e.ident;
    let struct_idents = || struct_variants_iter().map(|(_v, v_data)| &v_data.struct_ident);
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.
        let ts_path = gen_ts_folder().join(format!(
            "{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        let js_path = gen_ts_folder().join(format!(
            "{}ー{}ー{}.js",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        create_dir_all(ts_path.parent().unwrap()).unwrap();
        let mut ts = String::new();
        let mut js = String::new();
        let flat_variants = variants
            .iter()
            .filter_map(|v| match &v.data {
                VariantData::Transparent { types } => Some((
                    types,
                    v.const_ident.to_string(),
                    &v.variant.ident,
                    &v.variant.attrs,
                )),
                _ => None,
            })
            .map(|(types, const_ident, v_ident, attrs)| {
                (
                    types
                        .iter()
                        .map(|ty| napi_derive_backend::ty_to_ts_type(ty, false, false, false).0)
                        .chain((repr != DiscriminantRepr::None).then(|| {
                            format!(
                                "{{ {}: typeof {} }}",
                                discriminant_field_name_js_case.value(),
                                const_ident
                            )
                        }))
                        .join(" & "),
                    format_ident!("{}{}", enum_ident, v_ident),
                    attrs,
                )
            })
            .collect::<Vec<_>>();
        if export_variant_types.unwrap_or(true) {
            for (ty, ident, attrs) in flat_variants.iter() {
                write_docs(attrs, "", &mut ts);
                writeln!(ts, "export type {ident} = {ty}")
                    .expect("Failed to write to TS output file");
            }
        }
        write_docs(&e.attrs, "", &mut ts);
        writeln!(
            ts,
            "export type {} = {};",
            enum_ident,
            (struct_idents)()
                .map(|s| s.to_string().to_case(Case::Pascal))
                .chain(flat_variants.iter().map(|(ty, i, _)| {
                    if export_variant_types.unwrap_or(true) {
                        i.to_string()
                    } else {
                        ty.clone()
                    }
                }))
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
        let mut js_f = File::create(js_path).expect("Failed to open JS output file");
        js_f.write_all(js.as_bytes()).unwrap();
    }
    let const_idents = variants.iter().map(|v| &v.const_ident);
    let struct_const_idents = struct_variants_iter().map(|(v, _v_data)| &v.const_ident);
    let const_values = variants.iter().map(|v| &v.const_value);
    let ts_type_attrs = struct_variants_iter().map(|(v, _v_data)| {
        let const_ident = syn::LitStr::new(&format!("typeof {}", v.const_ident), Span::call_site());
        quote! {
            #[napi(ts_type = #const_ident)]
        }
    });
    let struct_fields = struct_variants_iter().map(|(_v, v_data)| {
        let mut pub_fields = v_data.fields.clone();
        for f in &mut pub_fields {
            f.vis = Visibility::Public(Pub {
                span: Span::call_site(),
            });
        }
        // The trailing comma is important, because we're adding one more field.
        if !pub_fields.empty_or_trailing() {
            pub_fields.push_punct(Comma {
                spans: [Span::call_site()],
            });
        }
        pub_fields
    });
    let struct_attrs = struct_variants_iter().map(|(v, _v_data)| {
        let mut attrs = v.variant.attrs.clone();
        attrs.retain(|a| a.path().get_ident().map(|i| i.to_string()).as_deref() != Some("enunion"));
        attrs
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
                        f.ident.as_ref().unwrap().to_string().to_case(Case::Camel)
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
                #[allow(non_snake_case)]
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
            a.path()
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
                            Some(#const_ident) => Ok(
                                <#struct_ident as Into<super::#enum_ident>>::into(
                                    #struct_ident::try_from(o)?
                                )
                            ),
                        }
                    }
                    VariantData::Transparent { types } => {
                        let matcher = types
                            .iter()
                            .enumerate()
                            .map(|(i, ty)| {
                                let ident = format_ident!("_{}", i);

                                quote! {
                                    let #ident = match <#ty as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val) {
                                        Ok(#ident) => #ident,
                                        Err(e) => break 'matcher Err(e),
                                    };
                                }
                            })
                            .collect::<proc_macro2::TokenStream>();

                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();

                        quote! {
                            Some(#const_ident) => {
                                let r = 'matcher: {
                                    #matcher
                                    Ok(super::#enum_ident::#v_ident ( #(#field_range),* ))
                                };

                                r.map_err(|e| ::napi::Error::from_reason(format!("JS object provided was not a valid {}, discriminant is {:?}, but encountered errors deserializing as that type: {:?}", stringify!(#enum_ident), ty, e)))
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
                            None
                        } else {
                            Some(quote! { { .. } })
                        };
                        let struct_ident = &data.struct_ident;
                        quote! {
                            val @ super::#enum_ident::#variant_ident #enum_pattern_tokens => <#struct_ident as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(__enunion_env, val.try_into().unwrap()),
                        }
                    }
                    VariantData::Transparent { types } => {
                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();
                        let const_ident = &v.const_ident;
                        quote! {
                            super::#enum_ident::#variant_ident(#(#field_range),*) => {
                                let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
                                let mut merged_object = env.create_object()?;
                                let sources = [#(
                                    unsafe {
                                        <::napi::JsObject as ::napi::NapiValue>::from_raw(
                                            __enunion_env,
                                            <#types as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(
                                                __enunion_env,
                                                #field_range
                                            )?).expect("enunion doesn't support intersection types where the members aren't objects")
                                    }
                                ),*];
                                ::enunion::merge_objects(&mut merged_object, &sources)?;
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

            #[allow(non_snake_case)]
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
                            _ => {
                                let json_value = &o;
                                let full_value = ::enunion::stringify_json_value(__enunion_env, json_value);
                                Err(::napi::Error::from_reason(format!("JS object provided was not a valid {}, {} = {:?} {}", stringify!(#enum_ident), #discriminant_field_name_js_case, ty, full_value)))
                            }
                        }
                    }
                }

                impl ::napi::bindgen_prelude::ValidateNapiValue for super::#enum_ident {}

                impl ::napi::bindgen_prelude::ToNapiValue for super::#enum_ident {
                    unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                        match val {
                            #into_arms
                        }
                    }
                }

                impl ::napi::bindgen_prelude::TypeName for super::#enum_ident {
                    fn type_name() -> &'static str {
                        stringify!(#enum_ident)
                    }

                    fn value_type() -> ::napi::bindgen_prelude::ValueType {
                        ::napi::bindgen_prelude::ValueType::Object
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
                    #[allow(non_snake_case)]
                    #[::napi_derive::napi(object)]
                    #(#struct_attrs)*
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
                #[allow(non_snake_case)]
                #[::napi_derive::napi(object)]
                #(#struct_attrs)*
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
        let from_impl = variants.iter()
            .map(|v| {
                match &v.data {
                    VariantData::Struct(s) => {
                        let s_ident = &s.struct_ident;
                        quote! {
                            match <#s_ident as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val) {
                                Ok(v) => {
                                    return Ok(<#s_ident as Into<super::#enum_ident>>::into(v));
                                },
                                Err(_) => {},
                            }
                        }
                    },
                    VariantData::Transparent { types } => {
                        let field_range = (0..types.len()).map(|i| format_ident!("_{}", i)).collect::<Vec<_>>();
                        let v_ident = &v.variant.ident;
                        let matches = types
                            .iter()
                            .enumerate()
                            .map(|(i, ty)| {
                                let ident = format_ident!("_{}", i);

                                quote! {
                                    let #ident = match <#ty as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val) {
                                        Ok(#ident) => #ident,
                                        Err(_) => break 'matcher,
                                    };
                                }
                            })
                            .collect::<proc_macro2::TokenStream>();

                        quote! {
                            'matcher: {
                                #matches
                                return Ok(super::#enum_ident::#v_ident ( #(#field_range),* ));
                            }
                        }
                    }
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
                                    let sources = [#(
                                        unsafe {
                                            <::napi::JsObject as ::napi::NapiValue>::from_raw(
                                                __enunion_env,
                                                <#types as ::napi::bindgen_prelude::ToNapiValue>::to_napi_value(
                                                    __enunion_env,
                                                    #field_range
                                                )?).expect("enunion doesn't support intersection types where the members aren't objects")
                                        }
                                    ),*];
                                    ::enunion::merge_objects(&mut merged_object, &sources)?;
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

            #[allow(non_snake_case)]
            #[doc(hidden)]
            mod #mod_ident {
                use ::napi::bindgen_prelude::*;
                use super::*;

                impl ::napi::bindgen_prelude::FromNapiValue for super::#enum_ident {
                    unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                        #from_impl;

                        let json_value = <::napi::JsUnknown as ::napi::NapiValue>::from_raw(__enunion_env, __enunion_napi_val).unwrap();
                        let full_value = ::enunion::stringify_json_value(__enunion_env, json_value);
                        Err(
                            ::napi::Error::from_reason(
                                format!(
                                    "JS object provided was not a valid {}, no variants deserialized correctly. {}",
                                    stringify!(#enum_ident),
                                    full_value
                                )
                            )
                        )
                    }
                }

                impl ::napi::bindgen_prelude::ValidateNapiValue for super::#enum_ident {}

                impl ::napi::bindgen_prelude::ToNapiValue for super::#enum_ident {
                    unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                        match val {
                            #into_arms
                        }
                    }
                }

                impl ::napi::bindgen_prelude::TypeName for super::#enum_ident {
                    fn type_name() -> &'static str {
                        stringify!(#enum_ident)
                    }

                    fn value_type() -> ::napi::bindgen_prelude::ValueType {
                        ::napi::bindgen_prelude::ValueType::Unknown
                    }
                }

                #struct_variants

                #[derive(Debug, Copy, Clone)]
                pub struct ContainedValueIsNotOfThatType;
            }
        }
    }.into()
}

fn write_docs(attrs: &[Attribute], prefix: &str, ts: &mut String) {
    let docs: Vec<&MetaNameValue> = attrs
        .iter()
        .filter_map(|a| {
            if a.path().get_ident()? != "doc" {
                return None;
            }
            match &a.meta {
                Meta::NameValue(m) => Some(m),
                _ => None,
            }
        })
        .collect::<Vec<_>>();
    if docs.is_empty() {
        return;
    }
    writeln!(ts, "{}/**", prefix).expect("Failed to write to TS output file");
    for doc in docs {
        if let Expr::Lit(ExprLit {
            lit: Lit::Str(s), ..
        }) = &doc.value
        {
            if s.value().is_empty() {
                writeln!(ts, "{} *", prefix).expect("Failed to write to TS output file");
            }
            for line in s.value().lines() {
                writeln!(ts, "{} * {}", prefix, line).expect("Failed to write to TS output file");
            }
        }
    }
    writeln!(ts, "{} */", prefix).expect("Failed to write to TS output file");
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
    fields: Punctuated<Field, Token!(,)>,
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
    ) -> Result<Self, (Span, String)> {
        if variant.discriminant.is_some() {
            return Err((variant.ident.span(), format!("Please use #[enunion(discriminant_value = \"value\") to specify the discriminant value, the ` = value` style on {}::{} is not supported", enum_ident, variant.ident)));
        }
        let const_ident = format_ident!(
            "{}_TYPE_{}",
            enum_ident.to_string().to_case(Case::UpperSnake),
            variant.ident.to_string().to_case(Case::UpperSnake)
        );
        let variant_args = variant
            .attrs
            .iter()
            .find(|a| a
                .path()
                .get_ident()
                .as_ref()
                .map(|&i| i == "enunion")
                .unwrap_or_default()
            )
            .map(|a| a.parse_args::<Args>().map_err(|e| (Span::call_site(), format!("Failed to parse enunion variant `{}` attribute input, please use field = \"value\" in a comma separated list. Check the documentation for examples. Error: {:?}", variant.ident, e))))
            .transpose()?;
        let mut discriminant_value = None;
        if let Some(variant_args) = variant_args {
            for arg in variant_args.items {
                match arg.path.get_ident().map(|i| i.to_string()).as_deref() {
                    Some("discriminant_value") => match arg.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => {
                            discriminant_value = Some(s.value());
                        }
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }) => {
                            discriminant_value = Some(i.base10_digits().to_string());
                        }
                        Expr::Lit(ExprLit {
                            lit: Lit::Bool(b), ..
                        }) => {
                            discriminant_value = Some(b.value.to_string());
                        }
                        _ => {
                            return Err((Span::call_site(), format!("literal type provided for {} is not supported, please use a string or an integer.", variant.ident)));
                        }
                    }
                    _ => return Err((Span::call_site(), format!("{} was not a recognized enunion argument, please use `discriminant_value`.", arg.path.to_token_stream())))
                }
            }
        }
        let (const_value, const_value_ts) = match repr {
            DiscriminantRepr::I64 => {
                let i: i64 = discriminant_value
                    .as_deref()
                    .map(|s| s
                        .parse()
                        .map_err(|e| (Span::call_site(), format!("discriminant_repr is i64, but discriminant_value is not an i64, this is not supported. {:?}", e))))
                    .transpose()?
                    .unwrap_or_else(|| variant_index.try_into().expect("too many variants!"));
                (quote! { #i }, i.to_string())
            }
            DiscriminantRepr::Bool => {
                let option_b: Option<bool> = discriminant_value
                    .as_deref()
                    .map(|s| -> Result<bool, _> { s
                        .parse()
                        .map_err(|e|
                            (Span::call_site(), format!("discriminant_repr is bool, but discriminant_value is not a bool, this is not supported. {:?}", e))
                        )
                    })
                    .transpose()?;
                let b = match option_b {
                    Some(b) => b,
                    None => {
                        let option_b = match variant_index {
                            0 => Some(false),
                            1 => Some(true),
                            _ => None,
                        };
                        option_b.ok_or_else(|| (Span::call_site(), String::from("only two variants are supported with discriminant_repr = \"bool\"")))?
                    }
                };
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
        let compute_struct_variant = |fields: Punctuated<Field, Token!(,)>| {
            let enum_field_tokens = fields
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

            let struct_ident = format_ident!("{}{}", enum_ident, variant.ident);
            VariantData::Struct(VariantStructData {
                fields,
                struct_ident,
                enum_field_tokens: quote! {
                    {
                        #enum_field_tokens
                    }
                },
                struct_field_tokens: quote! {
                    {
                        #struct_field_tokens
                    }
                },
            })
        };
        let data = match &variant.fields {
            Fields::Named(named) => (compute_struct_variant)(named.named.clone()),
            Fields::Unit => {
                if DiscriminantRepr::None == repr {
                    return Err((variant.ident.span(), String::from("discriminant_repr = \"none\" cannot be used with a unit variant. You must add fields.")));
                }
                (compute_struct_variant)(Punctuated::new())
            }
            Fields::Unnamed(unnamed) => VariantData::Transparent {
                types: unnamed.unnamed.iter().map(|f| f.ty.clone()).collect(),
            },
        };
        Ok(Self {
            variant,
            const_ident,
            const_value,
            const_value_ts,
            discriminant_value,
            data,
        })
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
    items: Punctuated<MetaNameValue, Token!(,)>,
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
    let mut str_literals = Vec::new();
    for v in e.variants.iter() {
        let variant_args = v
            .attrs
            .iter()
            .find(|a| {
                a.path()
                    .get_ident()
                    .as_ref()
                    .map(|&i| i == "string_enum")
                    .unwrap_or_default()
            })
            .map(|a| a.parse_args::<Args>())
            .transpose();
        let variant_args = match variant_args {
            Ok(o) => o,
            Err(e) => abort_call_site!("Failed to parse string_enum variant `{}` attribute input, please use field = \"value\" in a comma separated list. Check the documentation for examples. Error: {:?}", v.ident, e)
        };
        let mut key = None;
        let mut value = None;
        if let Some(variant_args) = variant_args {
            for arg in variant_args.items {
                match arg.path.get_ident().map(|i| i.to_string()).as_deref() {
                    Some("key") => match arg.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => {
                            key = Some(s.value());
                        }
                        _ => {
                            abort!(arg.value.span(), "literal type provided is not supported, please use a string.");
                        }
                    },
                    Some("value") => match arg.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => {
                            value = Some(s.value());
                        }
                        _ => {
                            abort!(arg.value.span(), "literal type provided is not supported, please use a string.");
                        }
                    },
                    _ => abort_call_site!(
                        "{} was not a recognized string_enum argument, please use `key` or `value`.",
                        arg.path.to_token_stream()
                    ),
                }
            }
        }
        str_literals.push((
            key.unwrap_or_else(|| v.ident.to_string()),
            value.unwrap_or_else(|| v.ident.to_string()),
        ));
    }
    let enum_ident = &e.ident;
    let variant_idents = e.variants.iter().map(|v| &v.ident).collect::<Vec<_>>();
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.

        // Add some leading underscores so these will sort to the top (important for JS code)
        let ts_path = gen_ts_folder().join(format!(
            "_{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));

        let js_path = gen_ts_folder().join(format!(
            "_{}ー{}ー{}.js",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        create_dir_all(ts_path.parent().unwrap()).unwrap();
        let mut ts = String::new();
        let mut js = String::new();
        write_docs(&e.attrs, "", &mut ts);
        writeln!(ts, "export enum {} {{", enum_ident).expect("Failed to write to TS output file");
        writeln!(
            js,
            "module.exports.{ident} = nativeBinding.{ident}",
            ident = enum_ident
        )
        .expect("Failed to write to TS output file");
        for (i, v) in e.variants.iter().enumerate() {
            write_docs(&v.attrs, "  ", &mut ts);
            let (key, value) = &str_literals[i];
            writeln!(ts, "  {ident} = \"{value}\",", ident = key, value = value)
                .expect("Failed to write to TS output file");
        }
        writeln!(ts, "}}").expect("Failed to write to TS output file");
        let mut ts_f = File::create(&ts_path).expect("Failed to open TS output file");
        ts_f.write_all(ts.as_bytes()).unwrap();
        let mut js_f = File::create(js_path).expect("Failed to open JS output file");
        js_f.write_all(js.as_bytes()).unwrap();
    }
    let (keys, values): (Vec<_>, Vec<_>) = str_literals.into_iter().unzip();
    let init_fn_ident = format_ident!(
        "____napi_register__enunion_{}",
        enum_ident.to_string().to_case(Case::Snake)
    );
    let cb_name = format_ident!(
        "__enunion_callback_{}",
        enum_ident.to_string().to_case(Case::Snake)
    );
    let mut e_altered = e.clone();
    // remove the `string_enum` attributes from the resulting enum, they've been parsed.
    for v in &mut e_altered.variants {
        v.attrs.retain(|a| {
            a.path()
                .segments
                .last()
                .map(|i| i.ident.to_string())
                .as_deref()
                != Some("string_enum")
        });
    }
    quote! {
        #e_altered

        impl ::napi::bindgen_prelude::FromNapiValue for #enum_ident {
            unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                let v = <String as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)?;
                match v.as_str() {
                    #(#values => Ok(#enum_ident::#variant_idents),)*
                    _ => Err(::napi::Error::from_reason(format!("JS string provided was not a valid {}, string is {:?}", stringify!(#enum_ident), v)))
                }
            }
        }

        impl ::napi::bindgen_prelude::ValidateNapiValue for #enum_ident {}

        impl ::napi::bindgen_prelude::ToNapiValue for #enum_ident {
            unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                let s = match &val {
                    #(#enum_ident::#variant_idents => #values,)*
                };
                ::napi::bindgen_prelude::ToNapiValue::to_napi_value(__enunion_env, s)
            }
        }

        impl ::napi::bindgen_prelude::TypeName for #enum_ident {
            fn type_name() -> &'static str {
                stringify!(#enum_ident)
            }

            fn value_type() -> ::napi::bindgen_prelude::ValueType {
                ::napi::bindgen_prelude::ValueType::String
            }
        }

        #[doc(hidden)]
        #[allow(non_snake_case)]
        #[allow(clippy::all)]
        unsafe fn #cb_name(__enunion_env: napi::sys::napi_env) -> napi::Result<napi::sys::napi_value> {
            let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
            let mut enum_object = env.create_object()?;
            #(
                enum_object.set(#keys, #values)?;
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

#[proc_macro]
#[proc_macro_error::proc_macro_error]
pub fn literal_typed_struct(item: TokenStream) -> TokenStream {
    let input_data = syn::parse::<FieldDescriptors>(item)
        .unwrap_or_else(|e| abort_call_site!("Failed to parse literal_typed_struct input {:?}", e));
    let name = input_data.struct_ident;
    let mod_name = format_ident!(
        "__enunion_literal_struct_{}",
        &&name.to_string().to_case(Case::Snake)
    );
    let fd_data = {
        let mut errs = Vec::new();
        let mut variants = Vec::new();
        let iter = input_data
            .values
            .iter()
            .map(FieldDescriptorData::new)
            .map(|r| match r {
                Ok(v) => variants.push(v),
                Err(e) => errs.push(e),
            });
        for _ in iter {}
        if !errs.is_empty() {
            let mut errs = errs.into_iter().flatten().peekable();
            while let Some(err) = errs.next() {
                if errs.peek().is_some() {
                    emit_error!(err.0, "{}", err.1);
                } else {
                    abort!(err.0, "{}", err.1);
                }
            }
            unreachable!()
        } else {
            variants
        }
    };
    let fields = fd_data.iter().map(|a| {
        let ts_type = LitStr::new(&a.value_ts, Span::call_site());

        let ident = &a.desc.ident;
        let ty = &a.desc.ty;

        quote! {
            #[napi(ts_type = #ts_type)]
            pub #ident: #ty
        }
    });
    let js_names = fd_data
        .iter()
        .map(|fd| {
            LitStr::new(
                &fd.desc.ident.to_string().to_case(Case::Camel),
                Span::call_site(),
            )
        })
        .collect::<Vec<_>>();
    let consts = fd_data.iter().map(|fd| &fd.desc.value).collect::<Vec<_>>();
    let get_field = fd_data.iter().zip(js_names.iter()).map(|(fd, js_name)| {
        let ty = &fd.desc.ty;
        match fd.repr {
            FieldDescriptorType::String => quote! {
                o.get::<_, #ty>(#js_name)?.as_deref()
            },
            _ => quote! {
                o.get::<_, #ty>(#js_name)?
            },
        }
    });
    let field_values = fd_data.iter().map(|fd| {
        let const_name = format_ident!("{}", fd.desc.ident.to_string().to_case(Case::UpperSnake));
        let fn_name = &fd.desc.ident;
        // This mess of code uses the given type, unless the representation is `String`, in which
        // case it uses &'static str instead.
        let const_type = match &fd.repr {
            FieldDescriptorType::String => Cow::Owned(Type::Reference(TypeReference {
                and_token: And {
                    spans: [Span::call_site()],
                },
                lifetime: Some(Lifetime::new("'static", Span::call_site())),
                mutability: None,
                elem: Box::new(Type::Path(TypePath {
                    qself: None,
                    path: Path::from(PathSegment::from(Ident::new("str", Span::call_site()))),
                })),
            })),
            _ => Cow::Borrowed(&fd.desc.ty),
        };
        let const_value = &fd.desc.value;
        quote! {
            pub const #const_name: #const_type = #const_value;

            pub const fn #fn_name(&self) -> #const_type {
                Self::#const_name
            }
        }
    });

    quote! {
        pub struct #name;

        impl ::napi::bindgen_prelude::FromNapiValue for #name {
            unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                let o = <::napi::JsObject as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)?;
                #(
                    match #get_field {
                        Some(value) => {
                            if !matches!(value, #consts) {
                                let json_value = &o;
                                let full_value = ::enunion::stringify_json_value(__enunion_env, json_value);
                                return Err(::napi::Error::from_reason(format!("Value \"{}\" was found, but it wasn't equal to {:?} {}", #js_names, #consts, full_value)));
                            }
                        }
                        None => {
                            let json_value = &o;
                            let full_value = ::enunion::stringify_json_value(__enunion_env, json_value);
                            return Err(::napi::Error::from_reason(format!("Value \"{}\" was undefined or null. {}", #js_names, full_value)));
                        }
                    }
                )*
                Ok(Self)
            }
        }

        impl ::napi::bindgen_prelude::ValidateNapiValue for #name {}

        impl ::napi::bindgen_prelude::ToNapiValue for #name {
            unsafe fn to_napi_value(__enunion_env: ::napi::sys::napi_env, val: Self) -> ::napi::bindgen_prelude::Result<::napi::sys::napi_value> {
                let env = unsafe { ::napi::Env::from_raw(__enunion_env) };
                let mut new_object = env.create_object()?;
                #(
                    new_object.set(#js_names, #consts)?;
                )*
                Ok(<::napi::JsObject as ::napi::NapiRaw>::raw(&new_object))
            }
        }

        impl ::napi::bindgen_prelude::TypeName for #name {
            fn type_name() -> &'static str {
                stringify!(#name)
            }

            fn value_type() -> ::napi::bindgen_prelude::ValueType {
                ::napi::bindgen_prelude::ValueType::Object
            }
        }

        impl #name {
            #(#field_values)*
        }

        #[allow(non_snake_case)]
        #[doc(hidden)]
        mod #mod_name {
            use super::*;

            #[allow(non_snake_case)]
            #[::napi_derive::napi(object)]
            pub struct #name {
                #(
                    #fields,
                )*
            }
        }
    }.into()
}

/// Takes a string literal and emits it verbatim into the output TypeScript file.
/// **This macro should only be used if no alternative is available. It comes with a high risk of
/// breaking your emitted TypeScript file because enunion cannot account for its usage, or reason
/// about it in any way.**
#[proc_macro]
#[proc_macro_error::proc_macro_error]
pub fn raw_ts(item: TokenStream) -> TokenStream {
    let input: LitStr = syn::parse(item)
        .unwrap_or_else(|e| abort_call_site!("raw_ts only accepts a string literal. {:?}", e));
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        let input = input.value();
        // Use a sha256 hash so that each invocation of the macro will have a stable prefix between
        // compilations. This should limit how often the output file changes order without reason.
        let mut hasher = sha2::Sha256::new();
        hasher.update(&input);

        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.
        let ts_path = gen_ts_folder().join(format!(
            "{}ー{}ーraw_tsー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            hex::encode(hasher.finalize()),
            // Use a uuid in order to distinguish the files in the event of a hash
            // collision.
            uuid::Uuid::new_v4(),
        ));
        create_dir_all(ts_path.parent().unwrap()).unwrap();
        let mut ts_f = File::create(&ts_path).expect("Failed to open TS output file");
        ts_f.write_all(input.as_bytes()).unwrap();
    }
    TokenStream::default()
}

struct FieldDescriptorData<'a> {
    pub desc: &'a FieldDescriptor,
    pub repr: FieldDescriptorType,
    pub value_ts: String,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum FieldDescriptorType {
    Bool,
    I64,
    String,
    Enum,
}

impl FieldDescriptorType {
    pub fn from_type(ty: &Type) -> Result<(Self, Option<PathSegment>), ProcMacroErrors> {
        let mut enum_name = None;
        let ret = match ty {
            Type::Path(TypePath {qself: _qself, path}) => {
                let ident = path.get_ident().map(|i| i.to_string());
                match ident.as_deref() {
                    Some("bool") => FieldDescriptorType::Bool,
                    Some("i64") => FieldDescriptorType::I64,
                    Some("String") => FieldDescriptorType::String,
                    _ => {
                        enum_name = path.segments.last().cloned();
                        FieldDescriptorType::Enum
                    }
                }
            }
            _ => return Err(vec![(Span::call_site(), String::from("literal_typed_struct only supports bool, i64, String, and enums in the type position."))])
        };
        Ok((ret, enum_name))
    }

    pub fn infer_from_expr(e: &Expr) -> Result<Self, ProcMacroErrors> {
        match e {
            Expr::Lit(ExprLit { lit, .. }) => match lit {
                Lit::Str(_) => Ok(Self::String),
                Lit::Int(_) => Ok(Self::I64),
                Lit::Bool(_) => Ok(Self::Bool),
                _ => Err(vec![(
                    e.span(),
                    format!("unsupported literal type {:?}", lit),
                )]),
            },
            Expr::Path(_) | Expr::Call(_) => Ok(Self::Enum),
            _ => Err(vec![(
                e.span(),
                String::from("Enunion was unable to infer the type of this expression"),
            )]),
        }
    }
}

impl<'a> FieldDescriptorData<'a> {
    pub fn new(desc: &'a FieldDescriptor) -> Result<Self, ProcMacroErrors> {
        let (repr, enum_name) = FieldDescriptorType::from_type(&desc.ty)?;
        let value_ts = ts_value(repr, &desc.ty, &desc.value, enum_name)?;
        Ok(Self {
            desc,
            repr,
            value_ts,
        })
    }
}

fn ts_value(
    repr: FieldDescriptorType,
    ty: &Type,
    value: &Expr,
    enum_name: Option<PathSegment>,
) -> Result<String, ProcMacroErrors> {
    let ret = match (repr, value) {
        (
            FieldDescriptorType::Bool,
            Expr::Lit(ExprLit {
                lit: Lit::Bool(lit),
                ..
            }),
        ) => lit.value.to_string(),
        (FieldDescriptorType::Bool, _) => {
            return Err(vec![(value.span(), String::from("bool type requested, but constant was not a bool. Please provide true or false."))]);
        }
        (
            FieldDescriptorType::I64,
            Expr::Lit(ExprLit {
                lit: Lit::Int(lit), ..
            }),
        ) => {
            if let Err(e) = str::parse::<i64>(lit.base10_digits()) {
                return Err(vec![(
                    value.span(),
                    format!("Provided integer could not be parsed as an i64 {:?}", e),
                )]);
            }
            lit.base10_digits().to_string()
        }
        (FieldDescriptorType::I64, _) => {
            return Err(vec![(
                value.span(),
                String::from(
                    "i64 type requested, but constant was not an i64. Please provide an integer.",
                ),
            )]);
        }
        (
            FieldDescriptorType::String,
            Expr::Lit(ExprLit {
                lit: Lit::Str(lit), ..
            }),
        ) => {
            format!("\"{}\"", lit.value())
        }
        (FieldDescriptorType::String, _) => {
            return Err(vec![(value.span(), String::from("String type requested, but value provided was not a string literal. Tip: Do not call String::from on a string literal, just provide the string literal."))]);
        }
        (FieldDescriptorType::Enum, Expr::Path(ExprPath { path, .. })) => {
            let enum_variant = path.segments.last().unwrap();
            format!("{}.{}", enum_name.unwrap().ident, enum_variant.ident)
        }
        // Support transparent enunion variants
        (FieldDescriptorType::Enum, Expr::Call(ExprCall { args, .. })) => {
            if args.len() == 1 {
                let inner_field_type = FieldDescriptorType::infer_from_expr(&args[0])?;
                let inner_type = match inner_field_type {
                    FieldDescriptorType::Enum => {
                        let path = match &args[0] {
                            Expr::Path(ExprPath { path, .. }) => path,
                            Expr::Call(ExprCall { func, .. }) => match &**func {
                                Expr::Path(ExprPath { path, .. }) => path,
                                _ => {
                                    return Err(vec![(
                                        args.span(),
                                        String::from("This type is not supported in this context"),
                                    )])
                                }
                            },
                            _ => {
                                return Err(vec![(
                                    args.span(),
                                    String::from("This type is not supported in this context"),
                                )])
                            }
                        };
                        if path.segments.len() < 2 {
                            return Err(vec![(
                                args.span(),
                                String::from("Unable to determine the type of this enum"),
                            )]);
                        }
                        Path {
                            leading_colon: path.leading_colon,
                            segments: path
                                .segments
                                .iter()
                                .take(path.segments.len() - 1)
                                .cloned()
                                .collect(),
                        }
                    }
                    _ => {
                        let ident = match inner_field_type {
                            FieldDescriptorType::Bool => "bool",
                            FieldDescriptorType::I64 => "i64",
                            FieldDescriptorType::String => "String",
                            FieldDescriptorType::Enum => unreachable!(),
                        };
                        Path::from(PathSegment {
                            ident: Ident::new(ident, Span::call_site()),
                            arguments: PathArguments::None,
                        })
                    }
                };
                let enum_name = inner_type.segments.last().cloned();
                return ts_value(
                    inner_field_type,
                    &Type::Path(TypePath {
                        qself: None,
                        path: inner_type,
                    }),
                    &args[0],
                    enum_name,
                );
            } else {
                return Err(vec![(args.span(), String::from("enunion only supports enum variants with one unnamed field in this context."))]);
            }
        }
        (FieldDescriptorType::Enum, _) => {
            return Err(vec![
                (ty.span(), String::from("Interpreting this type as an enum because it wasn't one of these types: bool, i64, String")),
                (value.span(), String::from("This is not a supported enum value"))
            ]);
        }
    };
    Ok(ret)
}

struct FieldDescriptor {
    pub ident: Ident,
    pub ty: Type,
    pub value: Expr,
}

impl Parse for FieldDescriptor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let _colon: Token!(:) = input.parse()?;
        let ty = input.parse()?;
        let _: Token!(=) = input.parse()?;
        let value = input.parse()?;
        Ok(Self { ident, ty, value })
    }
}

struct FieldDescriptors {
    struct_ident: Ident,
    values: Punctuated<FieldDescriptor, Token!(,)>,
}

impl Parse for FieldDescriptors {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let struct_ident = Ident::parse(input)?;
        let _ = Comma::parse(input)?;
        let values = Punctuated::<_, _>::parse_terminated(input)?;
        Ok(Self {
            struct_ident,
            values,
        })
    }
}

fn gen_ts_folder() -> PathBuf {
    PathBuf::from(var("CARGO_MANIFEST_DIR").unwrap()).join("enunion-generated-ts")
}
