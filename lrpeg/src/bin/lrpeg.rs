use lrpeg::build_parser;
use std::path::PathBuf;
use std::{env, fs};

fn main() {
    let mut args = env::args();

    args.next();

    let filename = args.next().unwrap();

    let src = fs::read_to_string(&filename).expect("failed to read input");

    let path = PathBuf::from(filename);

    println!(
        "{}",
        build_parser(&src, path.file_stem().unwrap().to_str().unwrap())
    );
}
