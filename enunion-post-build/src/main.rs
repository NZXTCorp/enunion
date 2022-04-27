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
    let read_dir_iter = match std::fs::read_dir("enunion-generated-ts") {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Could not read directory enunion-generated-ts, this may be because there was nothing to do. {:?}", e);
            return;
        }
    };
    let mut s = String::from("// -- BEGIN ENUNION GENERATED CODE --\n\n");
    for f in read_dir_iter {
        let c = std::fs::read_to_string(f.unwrap().path()).unwrap();
        writeln!(s, "{}", c).unwrap();
    }
    writeln!(s, "// -- END ENUNION GENERATED CODE --").unwrap();
    out_ts.write(s.as_bytes()).unwrap();
    std::fs::remove_dir_all("enunion-generated-ts").unwrap();
    println!("TypeScript files merged successfully!");
}
