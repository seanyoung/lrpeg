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
                    Rule::num => str::parse(node.children[0].as_str(input)).unwrap(),
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
