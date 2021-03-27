use crate::ast;
use std::collections::HashMap;

#[allow(clippy::all)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod peg;

#[derive(Debug)]
pub struct Definition {
    id: String,
    def: Expression,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Dot,
    Identifier(String),
    StringLiteral(String),
    Regex(String),
    Alternative(Box<Expression>, Box<Expression>),
    MustMatch(Box<Expression>),
    MustNotMatch(Box<Expression>),
    Optional(Box<Expression>),
    Any(Box<Expression>),
    More(Box<Expression>),
    List(Vec<Expression>),
}

pub fn parse(src: &str) -> ast::Grammar {
    let pt = peg::PEGParser::new().parse(&src).unwrap();

    let mut grammar = ast::Grammar {
        lookup: HashMap::new(),
        definitions: Vec::new(),
    };

    // pass one: fill in rule names
    for def in &pt {
        if grammar.lookup.contains_key(&def.id) {
            panic!("duplicate rule: {}", def.id);
        }

        grammar
            .lookup
            .insert(def.id.to_string(), grammar.definitions.len());

        grammar.definitions.push(ast::Definition {
            name: def.id.to_string(),
            sequence: ast::Expression::Dot,
        });
    }

    for (rule_no, def) in pt.into_iter().enumerate() {
        let sequence = expression(&def.def, &grammar);

        grammar.definitions[rule_no].sequence = sequence;
    }

    grammar
}

fn expression(expr: &Expression, grammar: &ast::Grammar) -> ast::Expression {
    match expr {
        Expression::Dot => ast::Expression::Dot,
        Expression::Identifier(name) => {
            if name == "WHITESPACE" {
                ast::Expression::Whitespace
            } else if name == "EOI" {
                ast::Expression::EOI
            } else if let Some(rule_no) = grammar.lookup.get(name) {
                ast::Expression::Definition(*rule_no)
            } else {
                panic!("rule {} not found", name);
            }
        }
        Expression::StringLiteral(s) => ast::Expression::StringLiteral(unquote(&s)),
        Expression::Regex(s) => ast::Expression::Regex(unquote(&s[1..])),
        Expression::MustMatch(expr) => {
            ast::Expression::MustMatch(Box::new(expression(expr, grammar)))
        }
        Expression::MustNotMatch(expr) => {
            ast::Expression::MustNotMatch(Box::new(expression(expr, grammar)))
        }
        Expression::Optional(expr) => {
            ast::Expression::Optional(Box::new(expression(expr, grammar)))
        }
        Expression::More(expr) => ast::Expression::More(Box::new(expression(expr, grammar))),
        Expression::Any(expr) => ast::Expression::Any(Box::new(expression(expr, grammar))),
        Expression::List(list) => {
            let list = list.iter().map(|expr| expression(expr, grammar)).collect();

            ast::Expression::List(list)
        }
        Expression::Alternative(left, right) => {
            let mut list = Vec::new();

            flatten_alternatives(left, right, &mut list);

            let list = list
                .into_iter()
                .map(|expr| expression(expr, grammar))
                .collect();

            ast::Expression::Alternatives(list)
        }
    }
}

fn flatten_alternatives<'a>(
    left: &'a Expression,
    right: &'a Expression,
    res: &mut Vec<&'a Expression>,
) {
    if let Expression::Alternative(left, right) = left {
        flatten_alternatives(left, right, res);
    } else {
        res.push(left);
    }

    if let Expression::Alternative(left, right) = right {
        flatten_alternatives(left, right, res);
    } else {
        res.push(right);
    }
}

fn unquote(src: &str) -> String {
    debug_assert!(src.len() > 2);

    let mut res = String::new();

    let mut chars = src[1..src.len() - 1].chars();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('t') => res.push('\t'),
                Some('n') => res.push('\n'),
                Some('r') => res.push('\r'),
                Some(ch) => res.push(ch),
                None => unreachable!(),
            }
        } else {
            res.push(ch);
        }
    }

    res
}
