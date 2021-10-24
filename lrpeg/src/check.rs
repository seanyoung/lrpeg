use std::collections::HashSet;

use super::ast;
use super::utils::KEYWORDS;

pub fn check_grammar(grammar: &mut ast::Grammar) {
    let mut used = HashSet::new();

    for def_no in 0..grammar.definitions.len() {
        let name = grammar.definitions[def_no].name.as_str();
        if KEYWORDS.contains(&name) {
            let mut name = name.to_owned();
            loop {
                name.insert(0, '_');
                if !grammar.definitions.iter().any(|def| def.name == name) {
                    break;
                }
            }
            grammar.definitions[def_no].name = name;
        }
    }

    for def in &grammar.definitions {
        check_expr(&def.sequence, grammar, &mut used);
    }

    let mut errors = 0;

    for (no, def) in grammar.definitions.iter().enumerate() {
        if no != 0 && !used.contains(&no) {
            eprintln!("rule {} is not used anywhere", def.name);
            errors += 1;
        }
    }

    assert!(errors == 0, "{} errors found", errors);
}

// just check for references for now
fn check_expr(expr: &ast::Expression, grammar: &ast::Grammar, used: &mut HashSet<usize>) {
    match &expr.expr {
        ast::BareExpression::MemoDefinition(no) | ast::BareExpression::Definition(no) => {
            used.insert(*no);
        }
        ast::BareExpression::List(list) | ast::BareExpression::Alternatives(list) => {
            for expr in list {
                check_expr(expr, grammar, used);
            }
        }
        ast::BareExpression::MustMatch(expr)
        | ast::BareExpression::MustNotMatch(expr)
        | ast::BareExpression::Optional(expr)
        | ast::BareExpression::Any(expr)
        | ast::BareExpression::More(expr) => {
            check_expr(expr.as_ref(), grammar, used);
        }
        ast::BareExpression::Dot
        | ast::BareExpression::Whitespace
        | ast::BareExpression::EndOfInput
        | ast::BareExpression::XidIdentifier
        | ast::BareExpression::StringLiteral(_)
        | ast::BareExpression::Regex(_) => (),
    }
}
