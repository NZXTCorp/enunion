pub use enunion_derive::*;
use napi::{Env, JsFunction, JsObject, JsStringUtf8, NapiRaw};

#[doc(hidden)]
pub unsafe fn stringify_json_value<V>(e: napi::sys::napi_env, json_value: V) -> String
where
    V: NapiRaw,
{
    let e = unsafe { Env::from_raw(e) };
    stringify_json_value_inner(&e, json_value)
        .as_ref()
        .and_then(|s| Some(format!("full value: {}", s.as_str().ok()?)))
        .unwrap_or_default()
}

fn stringify_json_value_inner<V>(e: &napi::Env, json_value: V) -> Option<JsStringUtf8>
where
    V: NapiRaw,
{
    let stringify = e
        .get_global()
        .and_then(|g| {
            g.get_named_property::<JsObject>("JSON")?
                .get::<_, JsFunction>("stringify")
        })
        .ok()??;

    stringify
        .call(None, &[json_value])
        .and_then(|s| s.coerce_to_string()?.into_utf8())
        .ok()
}

#[doc(hidden)]
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
