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
                    sequence: ast::Expression {
                        alt: None,
                        label: None,
                        expr: ast::BareExpression::Dot,
                    },
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
    let alternatives = collect_alternatives(node, src);

    let mut alts = Vec::new();

    for (label, alt) in alternatives {
        let mut list = Vec::new();

        for expr in collect_rules(alt, peg::Rule::alternative) {
            list.push(collect_alternative(expr, grammar, src));
        }

        alts.push(if list.len() == 1 {
            let mut e = list[0].clone();
            e.alt = label;
            e
        } else {
            ast::Expression {
                alt: label,
                label: None,
                expr: ast::BareExpression::List(list),
            }
        });
    }

    if alts.len() == 1 {
        alts[0].clone()
    } else {
        ast::Expression {
            alt: None,
            label: None,
            expr: ast::BareExpression::Alternatives(alts),
        }
    }
}

fn collect_alternative(node: &peg::Node, grammar: &ast::Grammar, src: &str) -> ast::Expression {
    assert_eq!(node.rule, peg::Rule::alternative);

    match node.alternative {
        Some("pub") => {
            let mut expr = collect_alternative(&node.children[1], grammar, src);
            expr.label = Some(
                node.children[1]
                    .as_str(src)
                    .chars()
                    .filter(|ch| ch.is_alphanumeric())
                    .collect(),
            );
            expr
        }
        Some("rename_pub") => {
            let mut expr = collect_alternative(&node.children[3], grammar, src);
            expr.label = Some(node.children[1].as_str(src).to_string());
            expr
        }
        Some("optional") => ast::BareExpression::Optional(Box::new(collect_alternative(
            &node.children[0],
            grammar,
            src,
        )))
        .into(),
        Some("any") => ast::BareExpression::Any(Box::new(collect_alternative(
            &node.children[0],
            grammar,
            src,
        )))
        .into(),
        Some("more") => ast::BareExpression::More(Box::new(collect_alternative(
            &node.children[0],
            grammar,
            src,
        )))
        .into(),
        Some("parenthesis") => collect_expr(&node.children[2], grammar, src),
        Some("primary") => collect_primary(&node.children[0], grammar, src),
        _ => unreachable!(),
    }
}

fn collect_primary(node: &peg::Node, grammar: &ast::Grammar, src: &str) -> ast::Expression {
    assert_eq!(node.rule, peg::Rule::primary);

    match node.alternative {
        Some("must") => ast::BareExpression::MustMatch(Box::new(collect_primary(
            &node.children[2],
            grammar,
            src,
        )))
        .into(),
        Some("must_not") => ast::BareExpression::MustNotMatch(Box::new(collect_primary(
            &node.children[2],
            grammar,
            src,
        )))
        .into(),
        Some("regex") => {
            ast::BareExpression::Regex(unquote(&node.children[0].as_str(src)[2..])).into()
        }
        Some("id") => match node.children[0].as_str(src) {
            "EOI" => ast::BareExpression::EndOfInput.into(),
            "WHITESPACE" => ast::BareExpression::Whitespace.into(),
            "XID_IDENTIFIER" => ast::BareExpression::XidIdentifier.into(),
            id => {
                if let Some(def_no) = grammar.lookup.get(id) {
                    ast::BareExpression::Definition(*def_no).into()
                } else {
                    panic!("rule {} not found", id);
                }
            }
        },
        Some("literal") => {
            ast::BareExpression::StringLiteral(unquote(node.children[0].as_str(src))).into()
        }
        Some("dot") => ast::BareExpression::Dot.into(),
        _ => unreachable!(),
    }
}

fn collect_alternatives<'a>(
    node: &'a peg::Node,
    src: &str,
) -> Vec<(Option<String>, &'a peg::Node)> {
    assert_eq!(node.rule, peg::Rule::expression);

    let mut alternatives = Vec::new();

    fn collect_alternatives<'a>(
        node: &'a peg::Node,
        alternatives: &mut Vec<(Option<String>, &'a peg::Node)>,
        src: &str,
    ) {
        for node in &node.children {
            assert_eq!(node.children[0].rule, peg::Rule::sequence_marker);
            assert_eq!(node.children[2].rule, peg::Rule::sequence);

            let label = if node.children[0].children.len() == 2 {
                Some(node.children[0].children[0].as_str(src).to_string())
            } else {
                None
            };

            alternatives.push((label, &node.children[2]));
        }
    }

    if node.alternative == Some("unlabeled") {
        alternatives.push((None, &node.children[0]));
        assert_eq!(node.children[1].rule, peg::Rule::Any);
        collect_alternatives(&node.children[1], &mut alternatives, src);
    } else {
        collect_alternatives(node, &mut alternatives, src);
    }

    alternatives
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
