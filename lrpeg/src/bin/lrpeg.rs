use lrpeg::{lr1, Generator};
use std::{env, fs};

fn main() {
    let mut args = env::args();

    args.next();

    let filename = args.next().unwrap();

    let src = fs::read_to_string(filename).expect("failed to read input");

    let grammar = lr1::parse(&src);

    let mut gen = Generator::new();

    println!("{}", gen.build(&grammar));
}
