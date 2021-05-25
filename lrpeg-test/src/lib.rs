#![cfg_attr(not(test), allow(dead_code, unused_imports))]

use std::env;
use std::path::PathBuf;

include!(concat!(env!("OUT_DIR"), "/calculator.rs"));
include!(concat!(env!("OUT_DIR"), "/direct_left_recursive.rs"));
include!(concat!(env!("OUT_DIR"), "/indirect_left_recursive.rs"));
include!(concat!(env!("OUT_DIR"), "/irp.rs"));
include!(concat!(env!("OUT_DIR"), "/repeat.rs"));
include!(concat!(env!("OUT_DIR"), "/test1.rs"));
include!(concat!(env!("OUT_DIR"), "/test2.rs"));
include!(concat!(env!("OUT_DIR"), "/lang.rs"));

#[test]
fn test1() {
    let mut p = test1::PEG::new();

    assert!(p.parse("barf").is_ok());
    assert!(p.parse("barf\n\r \t").is_ok());
    assert!(p.parse("berf").is_err());
}

#[test]
fn test2() {
    let mut p = test2::PEG::new();

    assert_eq!(
        test2::PEG::new()
            .parse("barf darf")
            .unwrap()
            .print_to_string("barf darf"),
        "(foo, alt=0, \"barf darf\", (Terminal, \"barf\"), (Terminal, \" \"), (Terminal, \"darf\"))"
    );

    assert!(p.parse("berf").is_err());

    // test dot. Also make sure that dot steps over non-ascii
    assert_eq!(
        test2::PEG::new()
            .parse("carf erf")
            .unwrap()
            .print_to_string("carf erf"),
        "(foo, alt=1, \"carf erf\", (carf, \"carf erf\", (Terminal, \"carf\"), (Dot, \" \"), (erf, \"erf\")))"
    );

    assert!(p.parse("carf erf").is_ok());
    assert!(p.parse("carfxerf").is_ok());
    assert!(p.parse("carf").is_err());
    assert!(p.parse("carfµ").is_err());
    assert!(p.parse("carfµerf").is_ok());
    assert!(p.parse("carfx").is_err());
}

#[test]
fn direct_left_recursive() {
    let mut p = direct_left_recursive::PEG::new();

    assert!(p.parse("1").is_ok());
    assert!(p.parse("0").is_err());
    assert!(p.parse("1+1").is_ok());
    assert!(p.parse("1+1+1").is_ok());
}

#[test]
fn indirect_left_recursive() {
    let mut p = indirect_left_recursive::PEG::new();

    assert!(p.parse("1").is_ok());
    assert!(p.parse("1-1").is_ok());
}

#[test]
fn calculator() {
    let mut p = calculator::PEG::new();

    let mut parse = |s: &str| -> String { p.parse(s).unwrap().print_to_string(s) };

    assert_eq!(
        parse("1+2*4"),
        "(expr, alt=0, \"1+2*4\", (expr, \"1\", (term, alt=3, \"1\", (num, \"1\"))), (Terminal, \"+\"), (term, alt=0, \"2*4\", (term, \"2\", (num, \"2\")), (Terminal, \"*\"), (num, \"4\")))"
    );

    assert_eq!(
        parse("1"),
        "(expr, alt=2, \"1\", (term, alt=3, \"1\", (num, \"1\")))"
    );
    assert_eq!(
        parse("1+1"),
        "(expr, alt=0, \"1+1\", (expr, \"1\", (term, alt=3, \"1\", (num, \"1\"))), (Terminal, \"+\"), (term, alt=3, \"1\", (num, \"1\")))"
    );
    assert_eq!(
        parse("(1+2)*4"),
        "(expr, alt=2, \"(1+2)*4\", (term, alt=0, \"(1+2)*4\", (term, \"(1+2)\", (Terminal, \"(\"), (expr, alt=0, \"1+2\", (expr, \"1\", (term, alt=3, \"1\", (num, \"1\"))), (Terminal, \"+\"), (term, alt=3, \"2\", (num, \"2\"))), (Terminal, \")\")), (Terminal, \"*\"), (num, \"4\")))"
    );
    assert_eq!(
        parse("1*(1+1"),
        "(expr, alt=2, \"1\", (term, alt=3, \"1\", (num, \"1\")))"
    );
}

#[test]
fn repeat() {
    let mut p = repeat::PEG::new();

    let mut parse = |s: &str| -> String { p.parse(s).unwrap().print_to_string(s) };

    assert_eq!(
        parse("abc"),
        "(foo, alt=0, \"abc\", (Terminal, \"a\"), (Terminal, \"b\"), (Terminal, \"c\"))"
    );
    assert_eq!(
        parse("ac"),
        "(foo, alt=0, \"ac\", (Terminal, \"a\"), (Terminal, \"\"), (Terminal, \"c\"))"
    );

    assert_eq!(
        parse("xyyyyz"),
        "(foo, alt=1, \"xyyyyz\", (Terminal, \"x\"), (Terminal, \"yyyy\", (Terminal, \"y\"), (Terminal, \"y\"), (Terminal, \"y\"), (Terminal, \"y\")), (Terminal, \"z\"))"
    );

    assert_eq!(
        parse("xz"),
        "(foo, alt=1, \"xz\", (Terminal, \"x\"), (Terminal, \"\"), (Terminal, \"z\"))"
    );

    assert_eq!(
        parse("def"),
        "(foo, alt=2, \"def\", (Terminal, \"d\"), (Terminal, \"e\", (Terminal, \"e\")), (Terminal, \"f\"))"
    );

    assert_eq!(
        parse("deeeef"),
        "(foo, alt=2, \"deeeef\", (Terminal, \"d\"), (Terminal, \"eeee\", (Terminal, \"e\"), (Terminal, \"e\"), (Terminal, \"e\"), (Terminal, \"e\")), (Terminal, \"f\"))"
    );

    assert_eq!(
        parse("kx"),
        "(foo, alt=3, \"kx\", (Terminal, \"k\"), (Terminal, \"\"), (Dot, \"x\"))"
    );

    assert_eq!(
        parse("qr"),
        "(foo, alt=4, \"qr\", (Terminal, \"q\"), (Terminal, \"\"), (Dot, \"r\"))"
    );

    assert_eq!(p.parse("qs").unwrap_err(), 1);
    assert_eq!(p.parse("kl").unwrap_err(), 0);
    assert_eq!(p.parse("df").unwrap_err(), 0);
    assert_eq!(p.parse("ad").unwrap_err(), 0);
}

#[test]
fn irp() {
    let mut p = irp::PEG::new();

    let mut parse = |s: &str| -> String { p.parse(s).unwrap().print_to_string(s) };

    parse("{37k,432}<1,-1|1,-3>(8,-4,67:8,83:8,X:4,D:4,S:8,F:8,T:8,1,-100,(8,-8,1,-100)*) {T=D+S:4:0+S:4:4+F:4:0+F:4:4} [D:0..15,S:0..255,F:0..255,X:0..15=1]");
    parse("{40k,520,msb}<1,-10|1,-1,1,-8>(S:1,<1:2|2:2>(F:D),-90m)*{D=7}[S:0..1,F:0..255]");
}

fn parse_lang(input: &str) -> Vec<usize> {
    let mut parser = lang::PEG::new();
    let mut res = Vec::new();

    match parser.parse(input) {
        Ok(node) => {
            for line_node in collect_rules(&node.children[0], lang::Rule::lines) {
                // do we have a keyword
                if !line_node.children[1].is_empty() {
                    let s = line_node.children[0].as_str(input).replace("\t", "    ");
                    res.push(s.len());
                }
            }
        }
        Err(pos) => panic!("parse error at {}", pos),
    }

    res
}

fn collect_rules(node: &lang::Node, rule: lang::Rule) -> Vec<&lang::Node> {
    let mut list = Vec::new();

    fn recurse<'t>(node: &'t lang::Node, rule: lang::Rule, list: &mut Vec<&'t lang::Node>) {
        if node.rule == rule {
            list.push(node);
        } else {
            for node in &node.children {
                recurse(node, rule, list);
            }
        }
    }

    recurse(node, rule, &mut list);

    list
}

#[test]
fn lang1() {
    let res = parse_lang(
        r#"
    c
  b
 a"#,
    );
    assert_eq!(res, vec![4, 2, 1]);
}

#[test]
#[should_panic(expected = "1 errors found")]
fn broken() {
    let out_dir = env::var("OUT_DIR").unwrap();
    lrpeg::process_files(&PathBuf::from("."), &PathBuf::from(out_dir));
}
