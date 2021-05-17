use lrpeg::{lr1, Generator};
use std::path::PathBuf;
use std::{env, fs};

fn main() {
    let mut args = env::args();

    args.next();

    let filename = args.next().unwrap();

    let src = fs::read_to_string(&filename).expect("failed to read input");

    let grammar = lr1::parse(&src);

    let mut gen = Generator::new();

    let path = PathBuf::from(filename);

    println!(
        "{}",
        gen.build(&grammar, path.file_stem().unwrap().to_str().unwrap())
    );
}
