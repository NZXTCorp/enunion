pub use enunion_derive::*;
use napi::{Env, JsFunction, JsObject, NapiRaw};
use std::fmt::Arguments;

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
