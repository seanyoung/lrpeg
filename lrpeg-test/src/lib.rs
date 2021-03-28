#![cfg_attr(not(test), allow(dead_code, unused_imports))]

mod calculator;
mod direct_left_recursive;
mod indirect_left_recursive;
mod irp;
mod repeat;
mod test1;
mod test2;

#[test]
fn test1() {
    let mut p = test1::PEG::new();

    assert!(p.parse("barf").is_ok());
    assert!(p.parse("berf").is_err());
}

#[test]
fn test2() {
    let mut p = test2::PEG::new();

    assert!(p.parse("barf darf").is_ok());
    assert!(p.parse("berf").is_err());

    // test dot. Also make sure that dot steps over non-ascii
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

    assert_eq!(parse("1"), "(Terminal, \"1\")");
    assert_eq!(
        parse("1+1"),
        "(term, \"1+1\", (Terminal, \"1\"), (Terminal, \"+\"), (Terminal, \"1\")))"
    );
    assert_eq!(
        parse("1*1"),
        "(expr, \"1*1\", (Terminal, \"1\"), (Terminal, \"*\"), (Terminal, \"1\")))"
    );
    assert_eq!(
        parse("1*100"),
        "(expr, \"1*100\", (Terminal, \"1\"), (Terminal, \"*\"), (Terminal, \"100\")))"
    );
    assert_eq!(parse("(1+1)"), "(term, \"(1+1)\", (Terminal, \"(\"), (term, \"1+1\", (Terminal, \"1\"), (Terminal, \"+\"), (Terminal, \"1\"))), (Terminal, \")\")))");
    assert_eq!(parse("1*(1+1"), "(Terminal, \"1\")");
}

#[test]
fn repeat() {
    let mut p = repeat::PEG::new();

    let mut parse = |s: &str| -> String { p.parse(s).unwrap().print_to_string(s) };

    assert_eq!(
        parse("abc"),
        "(foo, \"abc\", (Terminal, \"a\"), (Terminal, \"b\"), (Terminal, \"c\")))"
    );
    assert_eq!(
        parse("ac"),
        "(foo, \"ac\", (Terminal, \"a\"), (Terminal, \"\"), (Terminal, \"c\")))"
    );

    assert_eq!(
        parse("xyyyyz"),
        "(foo, \"xyyyyz\", (Terminal, \"x\"), (Terminal, \"yyyy\", (Terminal, \"y\"), (Terminal, \"y\"), (Terminal, \"y\"), (Terminal, \"y\"))), (Terminal, \"z\")))"
    );

    assert_eq!(
        parse("xz"),
        "(foo, \"xz\", (Terminal, \"x\"), (Terminal, \"\"), (Terminal, \"z\")))"
    );

    assert_eq!(
        parse("def"),
        "(foo, \"def\", (Terminal, \"d\"), (Terminal, \"e\", (Terminal, \"e\"))), (Terminal, \"f\")))"
    );

    assert_eq!(
        parse("deeeef"),
        "(foo, \"deeeef\", (Terminal, \"d\"), (Terminal, \"eeee\", (Terminal, \"e\"), (Terminal, \"e\"), (Terminal, \"e\"), (Terminal, \"e\"))), (Terminal, \"f\")))"
    );

    assert_eq!(
        parse("kx"),
        "(foo, \"kx\", (Terminal, \"k\"), (Terminal, \"\"), (Dot, \"x\")))"
    );

    assert_eq!(
        parse("qr"),
        "(foo, \"qr\", (Terminal, \"q\"), (Terminal, \"\"), (Dot, \"r\")))"
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

    assert_eq!(
        parse("{38.123k, 550 }"),
        "(irp, \"{38.123k, 550 }\", (general_spec, \"{38.123k, 550 }\", (WHITESPACE, \"\"), (Terminal, \"{\"), (WHITESPACE, \"\"), (general_item, \"38.123k\", (Terminal, \"38.123\"), (WHITESPACE, \"\"), (Terminal, \"k\"), (WHITESPACE, \"\"))), (Terminal, \", 550 \", (Terminal, \", 550 \", (Terminal, \",\"), (WHITESPACE, \" \"), (general_item, \"550 \", (Terminal, \"550\"), (WHITESPACE, \" \"))))))), (Terminal, \"}\"), (WHITESPACE, \"\"))), (Terminal, \"\"), (EOI, \"\")))"
    );

    assert_eq!(
        parse("{msb} [ foo:0..255 ]"),
        "(irp, \"{msb} [ foo:0..255 ]\", (general_spec, \"{msb} \", (WHITESPACE, \"\"), (Terminal, \"{\"), (WHITESPACE, \"\"), (general_item, \"msb\", (Terminal, \"msb\"), (WHITESPACE, \"\"))), (Terminal, \"\"), (Terminal, \"}\"), (WHITESPACE, \" \"))), (parameter_specs, \"[ foo:0..255 ]\", (Terminal, \"[\"), (WHITESPACE, \" \"), (parameter_spec, \"foo:0..255 \", (XID_IDENTIFIER, \"foo\"), (WHITESPACE, \"\"), (Terminal, \"\"), (WHITESPACE, \"\"), (Terminal, \":\"), (WHITESPACE, \"\"), (Terminal, \"0\"), (WHITESPACE, \"\"), (Terminal, \"..\"), (WHITESPACE, \"\"), (Terminal, \"255\"), (WHITESPACE, \" \"))), (Terminal, \"\"), (Terminal, \"]\"), (WHITESPACE, \"\"))), (EOI, \"\")))"
    );
}
