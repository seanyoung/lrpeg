use std::env;
use std::path::PathBuf;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    lrpeg::process_files(&PathBuf::from("src"), &PathBuf::from(out_dir));
}
