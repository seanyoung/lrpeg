
[![crates.io](https://img.shields.io/crates/v/lrpeg.svg)](https://crates.io/crates/lrpeg)
[![CI](https://github.com/seanyoung/lrpeg/workflows/test/badge.svg)](https://github.com/seanyoung/lrpeg/actions)
[![license](https://img.shields.io/github/license/seanyoung/lrpeg.svg)](LICENSE)

# Left Recursive Parsing Expression Grammar (PEG)

lrpeg allows left recursive rules, and uses ratpack parsing for speed. I wrote a
[blog post](https://www.mess.org/2021/03/26/Left-Recursive-PEG-Parser-Generator/) to introduce the ideas
of lrpeg.

The existing PEG parser generators for rust do not allow left recursion,
which makes it very awkward to write grammars. It is possible to write
a PEG parser generator which allows for
[left recursion](http://www.vpri.org/pdf/tr2007002_packrat.pdf),
just as [python now uses](https://medium.com/@gvanrossum_83706/left-recursive-peg-grammars-65dab3c580e1).

See [IRP Grammar](https://github.com/seanyoung/ir/blob/main/irp/src/irp.peg) for a complete lrpeg grammar and handling for
[IRP](http://hifi-remote.com/wiki/index.php?title=IRP_Notation).

## How to use lrpeg

Add lrpeg to your Cargo.toml in build-dependencies:

```
[build-dependencies]
lrpeg = "0"

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

num <- r"[0-9]+" WHITESPACE;
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
        Err(pos) => {
            eprintln!("parser error at offset {}", pos + 1);
        }
    }
}
```
This example is available in [lrpeg-example](https://github.com/seanyoung/lrpeg/tree/main/lrpeg-example/).

## How to write grammar

PEG grammars are a set of rules. In lrpeg, each rule must end with a ";". Parsing starts at the top rule.

- Each rule must start with an identifier, followed by `<-` and then the terms and terminated by `;`
- A term can be repeated with `*`, `+` or optional `?`.
- The list of terms cannot be empty (but a term can be optional)
- A text literal can be encoded in single or double quotes ("foo" or 'bar')
- A regex must be written as `r"[0-9]+"`. Backslashed must be escaped `r"\\d+"` (this really needs fixing)
- Alternatives are denoted with `/`. For example `foo <- "a" / "b";`
- There are special terms `WHITESPACE`, `EOI`, and `XID_IDENTIFIER` (for unicode identifiers)

## How lrpeg is bootstrapped

First of all, we need to bootstrap the PEG. We do this with a very simple
lalrpop LR(1) PEG grammar. This grammar has some limitations, but it is
good enough.

## TODO

- More tests
- Better parse error information (now only the error offset is returned)
- Better documentation
- Make generator into rust macro
- Detect unused rules
- Detect unreachable alternatives
