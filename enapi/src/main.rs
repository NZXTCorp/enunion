use std::env;
use std::fmt::Write as _;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    let is_build = args.first().map(|s| s == "build").unwrap_or(false);
    let is_platform = args.iter().any(|a| a.as_str() == "--platform");
    if is_build {
        let _ = std::fs::remove_dir_all("enunion-generated-ts");
    }
    let specified_directory = args.windows(2).find_map(|a| {
        if let &[first, second] = &a {
            (!first.starts_with("--") && !second.starts_with("--")).then(|| second.clone())
        } else {
            None
        }
    });
    let specified_dts_index = args
        .iter()
        .enumerate()
        .find_map(|(i, a)| (a.as_str() == "--dts").then_some(i));
    let specified_dts = specified_dts_index.and_then(|i| args.get(i + 1).cloned());
    let specified_js_index = args
        .iter()
        .enumerate()
        .find_map(|(i, a)| (a.as_str() == "--js").then_some(i));
    let specified_js = specified_js_index.and_then(|i| args.get(i + 1).cloned());
    #[cfg(target_os = "windows")]
    let build_exit_status = Command::new("powershell")
        .args(["-Command", "npx"])
        .arg("napi")
        .args(args)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    #[cfg(not(target_os = "windows"))]
    let build_exit_status = Command::new("npx")
        .arg("napi")
        .args(args)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if !is_build {
        return;
    }
    if build_exit_status.success() {
        let mut build_dir = PathBuf::from(".");
        if let Some(directory) = specified_directory {
            build_dir = build_dir.join(directory);
        }
        // Append new TS definitions
        let ts_file_path =
            build_dir.join(specified_dts.unwrap_or_else(|| "index.d.ts".to_string()));
        let mut out_ts = match std::fs::OpenOptions::new().append(true).open(&ts_file_path) {
            Ok(out_ts) => out_ts,
            Err(e) => {
                eprintln!("Could not open file {ts_file_path:?} {e:?}");
                std::process::exit(1);
            }
        };

        let mut out_js = is_platform.then(|| {
            std::fs::OpenOptions::new()
                .append(true)
                .open(specified_js.unwrap_or_else(|| "index.js".to_string()))
                .unwrap()
        });
        let read_dir_iter = match std::fs::read_dir("enunion-generated-ts") {
            Ok(i) => i,
            Err(e) => {
                eprintln!("Could not read directory enunion-generated-ts, this may be because there was nothing to do. {:?}", e);
                return;
            }
        };
        let mut read_dir_iter = read_dir_iter.collect::<Vec<_>>();
        // Sort in alphabetical order so that output order can be controlled with prefixes.
        read_dir_iter
            .sort_unstable_by_key(|d| d.as_ref().ok().map(|d| d.path().display().to_string()));
        let mut ts = String::from("// -- BEGIN ENUNION GENERATED CODE --\n\n");
        let mut js = out_js
            .as_ref()
            .map(|_| String::from("// -- BEGIN ENUNION GENERATED CODE --\n\n"));
        for f in read_dir_iter {
            let path = f.unwrap().path();
            let c = std::fs::read_to_string(&path).unwrap();
            if path.extension().and_then(|o| o.to_str()) == Some("ts") {
                writeln!(ts, "{}", c).unwrap();
            } else if let Some(js) = js.as_mut() {
                writeln!(js, "{}", c).unwrap()
            }
        }
        writeln!(ts, "// -- END ENUNION GENERATED CODE --").unwrap();
        out_ts.write_all(ts.as_bytes()).unwrap();
        if let Some(js) = js.as_mut() {
            writeln!(js, "// -- END ENUNION GENERATED CODE --").unwrap();
            out_js.as_mut().unwrap().write_all(js.as_bytes()).unwrap();
        }
        println!("TypeScript files merged successfully!");
    }
    let _ = std::fs::remove_dir_all("enunion-generated-ts");
    std::process::exit(build_exit_status.code().unwrap_or(1));
}
