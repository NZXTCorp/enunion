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
    Field, Fields, Ident, ItemEnum, Lit, MetaNameValue, Variant, VisPublic,
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
/// ```
/// cargo install enunion-post-build
/// ```
///
/// # Params
/// - `discriminant_repr`: The representation used by the discriminant field, either "i64" or "str". Default: "i64"
/// - `discriminant_field_name`: The name of the discriminant field. Can be overridden here if you don't like the default. Default: `<enum_name>_type`, where `<enum_name>` is the name of your enum.
/// This will be converted to lowerCamelCase in the TypeScript file.
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
                        other => {
                            abort_call_site!("{} is not a recognized representation, please use \"i64\" or \"str\"", other)
                        }
                    }
                }
                _ => abort_call_site!("only string literals are supported for the discriminant_repr, please provide \"i64\" or \"str\"")
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
    let repr = repr.unwrap_or(DiscriminantRepr::I64);
    let (discriminant_type, discriminant_type_dynamic) = match repr {
        DiscriminantRepr::I64 => (quote!(i64), quote!(i64)),
        DiscriminantRepr::String => (quote!(&'static str), quote!(String)),
    };

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
    let variants = e
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| VariantData::new(&e.ident, v, repr, &discriminant_field_name, i))
        .collect::<Vec<_>>();
    let mod_ident = format_ident!(
        "__enunion_{}",
        heck::AsSnekCase(e.ident.to_string()).to_string()
    );
    let enum_ident = &e.ident;
    let struct_idents = variants
        .iter()
        .map(|v| format_ident!("{}{}", enum_ident, v.variant.ident))
        .collect::<Vec<_>>();
    // This is the NAPI internal environment variable used to find the path to write TS definitions to. If it's set, then a new file is being generated.
    if var("TYPE_DEF_TMP_PATH").is_ok() {
        // Use of a CJK dash here is intentional, since it's not a character that can be used in a cargo package name.
        let path = PathBuf::from("enunion-generated-ts").join(&format!(
            "{}ー{}ー{}.d.ts",
            var("CARGO_PKG_NAME").unwrap(),
            var("CARGO_PKG_VERSION").unwrap(),
            enum_ident
        ));
        create_dir_all(path.parent().unwrap()).unwrap();
        let mut s = String::new();
        writeln!(
            s,
            "type {} = {};",
            enum_ident,
            struct_idents.iter().join(" | ")
        )
        .expect("Failed to write to TS output file");
        for v in &variants {
            writeln!(
                s,
                "export const {ident}: {value};",
                ident = v.const_ident,
                value = v.const_value_ts
            )
            .expect("Failed to write to TS output file");
        }
        let mut f = File::create(&path).expect("Failed to open TS output file");
        f.write_all(s.as_bytes()).unwrap();
    }
    let const_idents = variants.iter().map(|v| &v.const_ident).collect::<Vec<_>>();
    let const_values = variants.iter().map(|v| &v.const_value);
    let ts_type_attrs = variants.iter().map(|v| {
        let const_value = syn::LitStr::new(&v.const_value_ts.to_string(), Span::call_site());
        quote! {
            #[napi(ts_type = #const_value)]
        }
    });
    let struct_fields = variants.iter().map(|v| {
        let mut pub_fields = v.fields.clone();
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
    let variant_idents = variants
        .iter()
        .map(|v| &v.variant.ident)
        .collect::<Vec<_>>();
    let enum_field_idents = variants
        .iter()
        .map(|v| {
            v.fields
                .iter()
                .map(|f| f.ident.as_ref().expect("infallible"))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let enum_field_tokens = variants
        .iter()
        .map(|v| &v.enum_field_tokens)
        .collect::<Vec<_>>();
    let struct_field_tokens = variants.iter().map(|v| &v.struct_field_tokens);
    let js_object_field_tokens = variants
        .iter()
        .map(|v| {
            v.fields
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
        DiscriminantRepr::I64 => quote! { ty },
        DiscriminantRepr::String => quote! { ty.as_deref() },
    };
    let enum_pattern_tokens = variants.iter().map(|v| {
        if v.fields.is_empty() {
            proc_macro2::TokenStream::new()
        } else {
            quote! { { .. } }
        }
    });
    quote! {
        #e

        #[doc(hidden)]
        mod #mod_ident {
            impl ::napi::bindgen_prelude::FromNapiValue for super::#enum_ident {
                unsafe fn from_napi_value(__enunion_env: ::napi::sys::napi_env, __enunion_napi_val: ::napi::sys::napi_value) -> ::napi::bindgen_prelude::Result<Self> {
                    let o = <::napi::JsObject as ::napi::bindgen_prelude::FromNapiValue>::from_napi_value(__enunion_env, __enunion_napi_val)?;
                    let ty: Option<#discriminant_type_dynamic> = o.get(stringify!(#discriminant_field_name_js_case))?;
                    match #ty_compare_expr {
                        #(Some(#const_idents) => Ok(<#struct_idents as Into<super::#enum_ident>>::into(#struct_idents::try_from(o)?)),)*
                        _ => Err(::napi::Error::from_reason(format!("JS object provided was not a valid {}", stringify!(#enum_ident))))
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

                    fn try_from(s: super::#enum_ident) -> Result<Self, Self::Error> {
                        match s {
                            super::#enum_ident::#variant_idents #enum_field_tokens => Ok(Self #struct_field_tokens),
                            _ => Err(ContainedValueIsNotOfThatType),
                        }
                    }
                }

                impl TryFrom<::napi::JsObject> for #struct_idents {
                    type Error = ::napi::Error;

                    fn try_from(o: ::napi::JsObject) -> Result<Self, Self::Error> {
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
    .into()
}

struct VariantData<'a> {
    variant: &'a Variant,
    fields: Punctuated<Field, Comma>,
    const_ident: Ident,
    const_value: proc_macro2::TokenStream,
    const_value_ts: String,
    enum_field_tokens: proc_macro2::TokenStream,
    struct_field_tokens: proc_macro2::TokenStream,
}

impl<'a> VariantData<'a> {
    pub fn new(
        enum_ident: &Ident,
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
            Fields::Unnamed(_) => abort_call_site!("enunion is not compatible with tuple enum variants such as {}::{}, try using named fields instead.", enum_ident, variant.ident)
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
            DiscriminantRepr::String => (
                quote! { stringify!(#const_ident) },
                format!("\"{}\"", const_ident),
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
        Self {
            variant,
            fields,
            const_ident,
            const_value,
            const_value_ts,
            enum_field_tokens,
            struct_field_tokens,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum DiscriminantRepr {
    I64,
    String,
}

struct Args {
    items: Punctuated<MetaNameValue, Comma>,
}

impl Parse for Args {
    fn parse(input: ParseStream<'_>) -> syn::parse::Result<Self> {
        Punctuated::<_, _>::parse_terminated(input).map(|items| Args { items })
    }
}
