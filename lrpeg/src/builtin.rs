use crate::ast::{BareExpression, Expression};
use std::collections::BTreeMap;

pub(crate) fn push_builtins(used: &BTreeMap<Expression, String>, res: &mut String) {
    let builtins = Builtins::init();
    for rule in used.keys() {
        if let Some(s) = builtins.rules.get(&rule.expr) {
            res.push_str(s)
        }
    }
}

pub(crate) struct Builtins {
    rules: BTreeMap<BareExpression, &'static str>,
}

impl Builtins {
    fn init() -> Builtins {
        let mut rules: BTreeMap<BareExpression, &'static str> = Default::default();
        rules.insert(BareExpression::XidIdentifier, BUILTIN_XID_IDENTIFIER);
        rules.insert(BareExpression::Dot, BUILTIN_DOT);
        rules.insert(BareExpression::Whitespace, BUILTIN_WHITESPACE);
        rules.insert(BareExpression::EndOfInput, BUILTIN_EOI);
        Self { rules }
    }
}

const BUILTIN_WHITESPACE: &str = r#"

    fn builtin_whitespace(&self, pos: usize, input: &str, label: Option<&'static str>, alternative: Option<&'static str>) -> Result<Node, usize> {
        // TODO: is this worth caching?
        let mut chars = input[pos..].char_indices();
        let mut next_pos;

        loop {
            if let Some((off, ch)) = chars.next() {
                next_pos = pos + off;

                if !ch.is_whitespace() {
                    break;
                }
            } else {
                next_pos = input.len();
                break;
            }
        }

        Ok(Node::new(Rule::WHITESPACE, pos, next_pos, label, alternative))
    }"#;
const BUILTIN_EOI: &str = r#"

    fn builtin_eoi(&self, pos: usize, input: &str, label: Option<&'static str>, alternative: Option<&'static str>) -> Result<Node, usize> {
        // not worth caching
        if pos == input.len() {
            Ok(Node::new(Rule::EOI, pos, pos, label, alternative))
        } else {
            Err(pos)
        }
    }"#;
const BUILTIN_DOT: &str = r#"

    fn builtin_dot(&self, pos: usize, input: &str, label: Option<&'static str>, alternative: Option<&'static str>) -> Result<Node, usize> {
        // not worth caching
        let mut chars = input[pos..].char_indices();
        if chars.next().is_some() {
            if let Some((len, _)) = chars.next() {
                Ok(Node::new(Rule::Dot, pos, pos + len, label, alternative))
            } else {
                Ok(Node::new(Rule::Dot, pos, input.len(), label, alternative))
            }
        } else {
            Err(pos)
        }
    }"#;

const BUILTIN_XID_IDENTIFIER: &str = r#"

    fn builtin_xid_identifier(&mut self, pos: usize, input: &str, label: Option<&'static str>, alternative: Option<&'static str>) -> Result<Node, usize> {
        let key = (pos, Rule::XID_IDENTIFIER);

        if let Some(res) = self.rule_memo.get(&key) {{
            let mut res = res.clone();
            if let Ok(res) = &mut res {
                res.label = label;
                res.alternative = alternative;
            }
            return res;
        }}

        let mut chars = input[pos..].char_indices();
        let mut end = pos;
        let mut res = if let Some((_, ch)) = chars.next() {
            if UnicodeXID::is_xid_start(ch) || ch == '_' {
                while {
                    if let Some((off, ch)) = chars.next() {
                        end = pos + off;
                        UnicodeXID::is_xid_continue(ch)
                    } else {
                        false
                    }
                } {}

                Ok(Node::new(Rule::XID_IDENTIFIER, pos, end, label, alternative))
            } else {
                Err(pos)
            }
        } else {
            Err(pos)
        };

        self.rule_memo.insert(key, res.clone());

        if let Ok(res) = &mut res {
            res.label = label;
            res.alternative = alternative;
        }

        res
    }"#;
