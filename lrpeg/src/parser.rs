use super::ast;
use std::collections::BTreeMap;

include!("peg.rs");

pub fn parse(src: &str) -> ast::Grammar {
    let mut parser = peg::PEG::new();

    match parser.parse(src) {
        Ok(node) => {
            let definitions = collect_rules(&node, peg::Rule::definition);

            let mut grammar = ast::Grammar {
                lookup: BTreeMap::new(),
                definitions: Vec::new(),
            };

            for def in &definitions {
                let name = def.children[0].as_str(src);

                assert!(
                    !grammar.lookup.contains_key(name),
                    "duplicate rule {}",
                    name
                );

                grammar
                    .lookup
                    .insert(name.to_owned(), grammar.definitions.len());

                grammar.definitions.push(ast::Definition {
                    name: name.to_owned(),
                    sequence: ast::Expression::Dot,
                })
            }

            for (def_no, def) in definitions.iter().enumerate() {
                let sequence = collect_expr(&def.children[4], &grammar, src);

                grammar.definitions[def_no].sequence = sequence;
            }

            grammar
        }
        Err((line_no, col_no)) => {
            panic!("parse failure at {}:{}", line_no, col_no);
        }
    }
}

fn collect_expr(node: &peg::Node, grammar: &ast::Grammar, src: &str) -> ast::Expression {
    let alternatives = collect_rules(node, peg::Rule::sequence);

    let mut alts = Vec::new();

    for alt in alternatives {
        let mut list = Vec::new();

        for expr in collect_rules(alt, peg::Rule::alternative) {
            list.push(collect_alternative(expr, grammar, src));
        }

        alts.push(if list.len() == 1 {
            list[0].clone()
        } else {
            ast::Expression::List(list)
        });
    }

    if alts.len() == 1 {
        alts[0].clone()
    } else {
        ast::Expression::Alternatives(alts)
    }
}

fn collect_alternative(node: &peg::Node, grammar: &ast::Grammar, src: &str) -> ast::Expression {
    assert_eq!(node.rule, peg::Rule::alternative);

    match node.alternative {
        Some(0) => collect_alternative(&node.children[1], grammar, src),
        Some(1) => collect_alternative(&node.children[3], grammar, src),
        Some(2) => ast::Expression::Optional(Box::new(collect_alternative(
            &node.children[0],
            grammar,
            src,
        ))),
        Some(3) => ast::Expression::Any(Box::new(collect_alternative(
            &node.children[0],
            grammar,
            src,
        ))),
        Some(4) => ast::Expression::More(Box::new(collect_alternative(
            &node.children[0],
            grammar,
            src,
        ))),
        Some(5) => collect_expr(&node.children[2], grammar, src),
        Some(6) => collect_primary(&node.children[0], grammar, src),
        _ => unreachable!(),
    }
}

fn collect_primary(node: &peg::Node, grammar: &ast::Grammar, src: &str) -> ast::Expression {
    assert_eq!(node.rule, peg::Rule::primary);

    match node.alternative {
        Some(0) => {
            ast::Expression::MustMatch(Box::new(collect_primary(&node.children[2], grammar, src)))
        }
        Some(1) => ast::Expression::MustNotMatch(Box::new(collect_primary(
            &node.children[2],
            grammar,
            src,
        ))),
        Some(2) => ast::Expression::Regex(unquote(&node.children[0].as_str(src)[2..])),
        Some(3) => match node.children[0].as_str(src) {
            "EOI" => ast::Expression::EndOfInput,
            "WHITESPACE" => ast::Expression::Whitespace,
            "XID_IDENTIFIER" => ast::Expression::XidIdentifier,
            id => {
                if let Some(def_no) = grammar.lookup.get(id) {
                    ast::Expression::Definition(*def_no)
                } else {
                    panic!("rule {} not found", id);
                }
            }
        },
        Some(4) => ast::Expression::StringLiteral(unquote(node.children[0].as_str(src))),
        Some(5) => ast::Expression::Dot,
        _ => unreachable!(),
    }
}

fn collect_rules(node: &peg::Node, rule: peg::Rule) -> Vec<&peg::Node> {
    let mut list = Vec::new();

    fn recurse<'t>(node: &'t peg::Node, rule: peg::Rule, list: &mut Vec<&'t peg::Node>) {
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

fn unquote(src: &str) -> String {
    debug_assert!(src.len() > 2);

    let mut res = String::new();

    let quote = src.chars().next().unwrap(); // first character
    let mut chars = src[1..src.len() - 1].chars();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('t') => res.push('\t'),
                Some('n') => res.push('\n'),
                Some('r') => res.push('\r'),
                Some(ch) => {
                    if ch != quote {
                        res.push('\\');
                    }
                    res.push(ch);
                }
                None => unreachable!(),
            }
        } else {
            res.push(ch);
        }
    }

    res
}
