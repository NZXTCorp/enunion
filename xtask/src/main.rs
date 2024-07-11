use std::{env, path::PathBuf, process::ExitStatus};

#[tokio::main]
async fn main() -> Result<(), ExitStatus> {
    let current_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("enunion-test");

    enunion_helper::post_build(
        current_dir.clone(),
        current_dir.join("index.d.ts"),
        true,
        Some(current_dir.join("index.js")),
        true,
    )
    .await?;
    Ok(())
}
