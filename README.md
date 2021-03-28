# Left Recursive Parsing Expression Grammar (PEG)

The existing PEG parser generators for rust do not allow left recursion,
which makes it very awkward to write grammars. It is possible to write
a PEG parser generator which allows for
[left recursion](http://www.vpri.org/pdf/tr2007002_packrat.pdf). This
implementation uses a simpler algorithm to implement indirect left
recursion.

## How to use lrpeg

Add lrpeg to your Cargo.toml in build-dependencies:

```
[build-dependencies]
lrpeg = "0"
```

Now add a `build.rs` to the root of your project, containing:

```
use std::path::PathBuf;

fn main() {
    lrpeg::process_files(&PathBuf::from("src"));
}
```
Write your peg grammar, and put it in a file which ends with `.peg`, for example `src/calculator.peg`:

```
expr <- term "*" term
    / term "/" term
    / term;

term <- term "+" term
    / term "-" term
    / "(" expr ")"
    / num;

num <- r"[0-9]+";
```
When your run `cargo build`, `src/calculator.rs` will be generated. You need to include this module in
your project, and then you can instantiate the PEG like so:

``` rust
mod calculator;

fn main() {
   let mut parser = calculator::PEG::new();

   match parser.parse("10 + (100 * 9)") {
      Ok(s) => println!("parse tree: {}", s.print_to_string()),
      Err(pos) => println!("parse error at offset {}", pos);
   }
}
```

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