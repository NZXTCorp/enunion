use std::fmt::Write as _;
use std::io::Write;

fn main() {
    let mut out_ts = match std::fs::OpenOptions::new().append(true).open("index.d.ts") {
        Ok(out_ts) => out_ts,
        Err(e) => {
            eprintln!("Could not open file index.d.ts {:?}", e);
            std::process::exit(1);
        }
    };

    let mut out_js = match std::fs::OpenOptions::new().append(true).open("index.js") {
        Ok(out_js) => out_js,
        Err(e) => {
            eprintln!("Could not open file index.js {:?}", e);
            std::process::exit(1);
        }
    };
    let read_dir_iter = match std::fs::read_dir("enunion-generated-ts") {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Could not read directory enunion-generated-ts, this may be because there was nothing to do. {:?}", e);
            return;
        }
    };
    let mut ts = String::from("// -- BEGIN ENUNION GENERATED CODE --\n\n");
    let mut js = String::from("// -- BEGIN ENUNION GENERATED CODE --\n\n");
    for f in read_dir_iter {
        let path = f.unwrap().path();
        let c = std::fs::read_to_string(&path).unwrap();
        if path.extension().and_then(|o| o.to_str()) == Some("ts") {
            writeln!(ts, "{}", c).unwrap();
        } else {
            writeln!(js, "{}", c).unwrap();
        }
    }
    writeln!(ts, "// -- END ENUNION GENERATED CODE --").unwrap();
    out_ts.write_all(ts.as_bytes()).unwrap();
    writeln!(js, "// -- END ENUNION GENERATED CODE --").unwrap();
    out_js.write_all(js.as_bytes()).unwrap();
    std::fs::remove_dir_all("enunion-generated-ts").unwrap();
    println!("TypeScript files merged successfully!");
}
