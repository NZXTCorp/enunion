pub use enunion_derive::*;
use napi::{Env, JsFunction, JsObject, NapiRaw};
use std::fmt::Arguments;

/// This macro is applied to Rust enums. It generates code that will expose the enum to TypeScript as a discriminated union. It uses `napi` to accomplish this.
/// Enunion also handles automatically converting between the two representations, in Rust you can define `#[napi]` methods that accept the enum as an argument, or return an instance of that enum.
/// The TypeScript will be free to handle it as a discriminated union, the Rust can handle it as an enum, and enunion will take care of translating at the boundary.
///
/// **This macro will not work if `napi` and `napi_derive` are not specified in the `[dependencies]` section of the `Cargo.toml`.**
///
///This macro has a companion library called `enunion-helper` which is appending the generated code from enunion to index.d.ts:
///
///
/// use enunion_helper;
/// let enunion_targer_dir = "path/to/generated/directory";
/// let ts_dest_path = "path/to/dest/index.d.ts";
/// enunion_helper::post_build(enunion_targer_dir.into(), ts_dest_path.into());
///
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
///
/// use enunion::enunion;
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
///

#[doc(hidden)]
pub unsafe fn stringify_json_value<V>(e: napi::sys::napi_env, json_value: V) -> String
where
    V: NapiRaw,
{
    let e = unsafe { Env::from_raw(e) };
    stringify_fn(&e)
        .and_then(|f| {
            f.call(None, &[json_value])
                .and_then(|s| s.coerce_to_string()?.into_utf8())
                .ok()
                .as_ref()
                .and_then(|s| Some(format!("full value: {}", s.as_str().ok()?)))
        })
        .unwrap_or_default()
}

fn stringify_fn(e: &napi::Env) -> Option<JsFunction> {
    e.get_global()
        .and_then(|g| {
            g.get_named_property::<JsObject>("JSON")?
                .get::<_, JsFunction>("stringify")
        })
        .ok()?
}

#[doc(hidden)]
#[inline]
pub fn merge_objects(target: &mut JsObject, sources: &[JsObject]) -> napi::Result<()> {
    for source in sources {
        for key in JsObject::keys(source)? {
            target.set_named_property::<napi::JsUnknown>(
                &key,
                source.get_named_property::<napi::JsUnknown>(&key)?,
            )?;
        }
    }

    Ok(())
}

#[doc(hidden)]
pub unsafe fn intersection_type_as_js_object<T>(
    env: napi::sys::napi_env,
    val: T,
) -> napi::Result<JsObject>
where
    T: napi::bindgen_prelude::ToNapiValue,
{
    let obj =
        unsafe { <napi::JsObject as napi::NapiValue>::from_raw(env, T::to_napi_value(env, val)?) }
            .expect("enunion doesn't support intersection types where the members aren't objects");
    Ok(obj)
}

#[doc(hidden)]
#[cold]
pub fn fail_discriminant_msg(
    enum_ty: &str,
    discriminant: &Arguments<'_>,
    err: &napi::Error,
) -> napi::Error {
    let reason = format!("Provided object was not a valid {enum_ty}, discriminant is {discriminant}, but encountered an error deserializing as that type: {err}");
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_on_wrong_discriminant(ty: &str) -> napi::Error {
    let reason = format!("Provided object was not {ty}");
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_string_enum_discriminant(string_enum_ty: &str, provided: &str) -> napi::Error {
    let reason =
        format!("String provided was not a valid {string_enum_ty}, string is `{provided}`");
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_on_missing_field(ty: &str, missing_field: &str) -> napi::Error {
    let reason = format!("Conversion to {ty} failed, field {missing_field} is missing");
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_invalid_ty_msg(
    enum_ty: &str,
    discriminant_field: &str,
    discriminant: &Arguments<'_>,
    val: &str,
) -> napi::Error {
    let reason = format!(
        "Provided object was not a valid {enum_ty}, {discriminant_field} = {discriminant:?} {val}"
    );
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_incorrect_literal(
    js_name: &str,
    constant_val: &Arguments<'_>,
    val: &str,
) -> napi::Error {
    let reason =
        format!("Value `{js_name}` was found, but it wasn't equal to {constant_val:?} {val}");
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_nullish(js_name: &str, val: &str) -> napi::Error {
    let reason = format!("Value `{js_name}` was undefined or null. {val}");
    napi::Error::from_reason(reason)
}

#[doc(hidden)]
#[cold]
pub fn fail_struct_ident(js_name: &str, val: &str) -> napi::Error {
    let reason = format!("Value `{js_name}` was undefined or null. {val}");
    napi::Error::from_reason(reason)
}
