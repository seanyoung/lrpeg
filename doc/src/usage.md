# How to use lrpeg from your code

Add lrpeg to your Cargo.toml in build-dependencies:

```
[build-dependencies]
lrpeg = "0.4"

[dependencies]
regex = "1"
unicode-xid = "0.2"
```

Now add a `build.rs` to the root of your project, containing:

```
use std::env;
use std::path::PathBuf;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    lrpeg::process_files(&PathBuf::from("src"), &PathBuf::from(out_dir));
}
```
Write your peg grammar, and put it in a file which ends with `.peg`, for example
[src/calculator.peg](https://github.com/seanyoung/lrpeg/tree/main/lrpeg-example/src/calculator.peg):

```
calculator <- expr EOI;

expr <- expr ("+" / "-") WHITESPACE term
    / term;

term <- term ("*" / "/" / "%") WHITESPACE  factor
    / factor;

factor <- "(" WHITESPACE  expr ")" WHITESPACE
    / num;

num <- re#[0-9]+# WHITESPACE;
```
When your run `cargo build`, `calculator.rs` will be generated into your `target/...` directory. You need
to include this module in your project, and then you can instantiate the PEG like so:

``` rust
include!(concat!(env!("OUT_DIR"), "/calculator.rs"));

use calculator::{Node, Rule};

fn main() {
    let mut parser = calculator::PEG::new();

    let args: Vec<String> = std::env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage {} EXPRESSION", &args[0]);
        std::process::exit(2);
    }

    let input = &args[1];

    println!("parsing: {}", input);

    match parser.parse(input) {
        Ok(node) => {
            fn walk(node: &Node, input: &str) -> u64 {
                match node.rule {
                    Rule::num => u64::from_str_radix(node.children[0].as_str(input), 10).unwrap(),
                    Rule::expr => {
                        if node.alternative == Some(0) {
                            let left = walk(&node.children[0], input);
                            let right = walk(&node.children[3], input);

                            match node.children[1].as_str(input) {
                                "+" => left + right,
                                "-" => left - right,
                                _ => unreachable!(),
                            }
                        } else {
                            walk(&node.children[0], input)
                        }
                    }
                    Rule::term => {
                        if node.alternative == Some(0) {
                            let left = walk(&node.children[0], input);
                            let right = walk(&node.children[3], input);

                            match node.children[1].as_str(input) {
                                "*" => left * right,
                                "/" => left / right,
                                "%" => left % right,
                                _ => unreachable!(),
                            }
                        } else {
                            walk(&node.children[0], input)
                        }
                    }
                    Rule::factor => {
                        if node.alternative == Some(0) {
                            walk(&node.children[2], input)
                        } else {
                            walk(&node.children[0], input)
                        }
                    }
                    Rule::calculator => walk(&node.children[0], input),
                    _ => {
                        unreachable!()
                    }
                }
            }

            println!("result: {}", walk(&node, input));
        }
        Err((line_no, col_no)) => {
            eprintln!("parser error at {}:{}", line_no, col_no);
        }
    }
}
```
This example is available in [lrpeg-example](https://github.com/seanyoung/lrpeg/tree/main/lrpeg-example/).
