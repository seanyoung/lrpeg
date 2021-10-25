use std::collections::{BTreeMap, HashSet};
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use unicode_xid::UnicodeXID;

pub mod ast;
mod check;
pub mod parser;
mod utils;

use utils::{escape_char, escape_string, KEYWORDS};

/// Generate a parser module for the peg described in source. The result is
/// a rust module as String.
pub fn build_parser(source: &str, modname: &str) -> String {
    let mut gen = Generator::new();

    gen.build(source, modname)
}

struct Generator {
    symbols: HashSet<String>,
    builtins: BTreeMap<ast::Expression, String>,
    terminals: BTreeMap<ast::Expression, String>,
}

impl Default for Generator {
    fn default() -> Self {
        Self::new()
    }
}

impl Generator {
    fn new() -> Self {
        Generator {
            symbols: HashSet::new(),
            builtins: BTreeMap::new(),
            terminals: BTreeMap::new(),
        }
    }

    fn build(&mut self, source: &str, modname: &str) -> String {
        let mut grammar = parser::parse(source);

        check::check_grammar(&mut grammar);

        // prepopulate builtins
        self.symbols.insert(String::from("Dot"));
        self.symbols.insert(String::from("WHITESPACE"));
        self.symbols.insert(String::from("EOI"));
        self.symbols.insert(String::from("XID_IDENTIFIER"));

        // collect all symbols
        for def in &grammar.definitions {
            self.symbols.insert(def.name.to_string());
        }

        for rule in &grammar.definitions {
            self.collect_terminals_recursive(&rule.sequence);
        }

        self.emit(&grammar, modname)
    }

    fn terminal_to_identifier(&mut self, s: &str, default: &str) -> String {
        let mut res = String::new();

        for ch in s.chars() {
            if res.is_empty() {
                if UnicodeXID::is_xid_start(ch) {
                    res.push(ch);
                }
            } else if UnicodeXID::is_xid_continue(ch) {
                res.push(ch);
            }
        }

        if res.is_empty() {
            res = String::from(default);
        }

        if self.symbols.contains(&res) {
            let mut suffix = 0;

            loop {
                let res_suffix = format!("{}_{}", res, suffix);
                if !self.symbols.contains(&res_suffix) {
                    res = res_suffix;
                    break;
                }

                suffix += 1;
            }
        }

        if KEYWORDS.contains(&res.as_str()) {
            res.insert(0, '_');
        }

        self.symbols.insert(res.to_owned());

        res
    }

    fn collect_terminals_recursive(&mut self, expr: &ast::Expression) {
        match &expr.expr {
            ast::BareExpression::StringLiteral(s) => {
                if !self.terminals.contains_key(expr) {
                    let s = self.terminal_to_identifier(s, "Literal");

                    self.terminals.insert(expr.clone(), s);
                }
            }
            ast::BareExpression::Regex(s) => {
                if !self.terminals.contains_key(expr) {
                    let s = self.terminal_to_identifier(s, "Regex");

                    self.terminals.insert(expr.clone(), s);
                }
            }
            ast::BareExpression::MustMatch(expr)
            | ast::BareExpression::MustNotMatch(expr)
            | ast::BareExpression::Optional(expr)
            | ast::BareExpression::Any(expr)
            | ast::BareExpression::More(expr) => {
                self.collect_terminals_recursive(expr);
            }
            ast::BareExpression::List(list) | ast::BareExpression::Alternatives(list) => {
                for expr in list {
                    self.collect_terminals_recursive(expr);
                }
            }
            ast::BareExpression::Dot => {
                self.builtins.insert(expr.clone(), String::from("Dot"));
            }
            ast::BareExpression::Whitespace => {
                self.builtins
                    .insert(expr.clone(), String::from("WHITESPACE"));
            }
            ast::BareExpression::EndOfInput => {
                self.builtins.insert(expr.clone(), String::from("EOI"));
            }
            ast::BareExpression::XidIdentifier => {
                self.builtins
                    .insert(expr.clone(), String::from("XID_IDENTIFIER"));
            }
            ast::BareExpression::MemoDefinition(_) | ast::BareExpression::Definition(_) => (),
        }
    }

    fn is_rule_direct_left_recursive(
        rule_no: usize,
        expr: &ast::Expression,
        examined_rules: &mut Vec<usize>,
        grammar: &ast::Grammar,
    ) -> bool {
        match &expr.expr {
            ast::BareExpression::Alternatives(list) => list.iter().any(|expr| {
                Generator::is_rule_direct_left_recursive(rule_no, expr, examined_rules, grammar)
            }),
            ast::BareExpression::List(list) => {
                Generator::is_rule_direct_left_recursive(rule_no, &list[0], examined_rules, grammar)
            }
            ast::BareExpression::Definition(no) if rule_no == *no => true,
            ast::BareExpression::Definition(no) => {
                if !examined_rules.contains(no) {
                    examined_rules.push(*no);
                    Generator::is_rule_direct_left_recursive(
                        rule_no,
                        &grammar.definitions[*no].sequence,
                        examined_rules,
                        grammar,
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn emit_rule(&self, def: &ast::Definition, grammar: &ast::Grammar) -> String {
        let mut res = format!(
            r#"

    #[allow(non_snake_case)]
    fn rule_{}(&mut self, pos: usize, input: &str) -> Result<Node, usize> {{
        let key = (pos, Rule::{});

        if let Some(res) = self.rule_memo.get(&key) {{
            return res.clone();
        }}

        let res = "#,
            def.name, def.name,
        );

        res.push_str(&self.emit_expr(&def.sequence, Some(&def.name), &None, grammar));

        res.push_str(
            r#";

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }"#,
        );

        res
    }

    #[allow(clippy::branches_sharing_code)]
    fn emit_left_recursive_rule(
        &self,
        rule_no: usize,
        def: &ast::Definition,
        grammar: &ast::Grammar,
    ) -> String {
        let mut res = format!(
            r#"

    #[allow(non_snake_case)]
    fn rule_{}(&mut self, pos: usize, input: &str) -> Result<Node, usize> {{
        let key = (pos, Rule::{});

        if let Some(res) = self.rule_memo.get(&key) {{
            return res.clone();
        }}

        self.rule_memo.insert(key, Err(pos));"#,
            def.name, def.name,
        );

        // first emit the non-left recursive alternatives
        if let ast::BareExpression::Alternatives(list) = &def.sequence.expr {
            res.push_str(
                r#"

        let mut res = Err(pos);
        let mut next_pos = pos;

        loop {
            let r = "#,
            );

            let mut list = list.clone();

            for entry in list.iter_mut() {
                if Generator::is_rule_direct_left_recursive(
                    rule_no,
                    entry,
                    &mut Vec::new(),
                    grammar,
                ) {
                    if let ast::BareExpression::List(list) = &mut entry.expr {
                        if let ast::BareExpression::Definition(rule_no) = list[0].expr {
                            list[0] = ast::BareExpression::MemoDefinition(rule_no).into();
                        }
                    }
                }
            }

            res.push_str(&self.emit_expr(
                &ast::BareExpression::Alternatives(list).into(),
                Some(&def.name),
                &None,
                grammar,
            ));
        } else {
            res.push_str(
                r#"

        let mut res = Err(pos);
        let mut next_pos = pos;

        loop {
            let r = "#,
            );
            res.push_str(&self.emit_expr(&def.sequence, Some(&def.name), &None, grammar));
        }

        res.push_str(
            r#";

            if let Ok(new_node) = &r {
                if new_node.end > next_pos {
                    next_pos = new_node.end;
                    res = r;
                    self.rule_memo.insert(key, res.clone());
                    continue;
                }
            }

            break;
        }

        self.rule_memo.insert(key, res.clone());

        res
    }"#,
        );

        res
    }

    fn emit_expr(
        &self,
        expr: &ast::Expression,
        rule: Option<&str>,
        alt: &Option<String>,
        grammar: &ast::Grammar,
    ) -> String {
        match &expr.expr {
            ast::BareExpression::List(list) => {
                let mut iter = list.iter();

                let mut last = iter.next().unwrap();

                let mut res = format!(
                    r#"{{
            let mut list = Vec::new();
            let start = pos;

            {}"#,
                    self.emit_expr(last, None, &None, grammar)
                );

                for expr in iter {
                    if last.label.is_some() {
                        res.push_str(&format!(
                            r#"
            .and_then(|mut node| {{
                let pos = node.end;
                node.label = {};
                list.push(node);
                {}
            }})"#,
                            option_stringify(&last.label),
                            self.emit_expr(expr, None, &None, grammar),
                        ))
                    } else {
                        res.push_str(&format!(
                            r#"
            .and_then(|node| {{
                let pos = node.end;
                list.push(node);
                {}
            }})"#,
                            self.emit_expr(expr, None, &None, grammar),
                        ))
                    };

                    last = expr;
                }

                if last.label.is_some() {
                    res.push_str(&format!(
                        r#"
            .map(|mut node| {{
                let end = node.end;
                node.label = {};
                list.push(node);

                Node {{
                    rule: Rule::{},
                    start,
                    end,
                    children: list,
                    label: {},
                    alternative: {},
                }}
            }})
        }}"#,
                        option_stringify(&last.label),
                        rule.unwrap_or("List"),
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    ));
                } else {
                    res.push_str(&format!(
                        r#"
            .map(|node| {{
                let end = node.end;
                list.push(node);

                Node {{
                    rule: Rule::{},
                    start,
                    end,
                    children: list,
                    label: {},
                    alternative: {},
                }}
            }})
        }}"#,
                        rule.unwrap_or("List"),
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    ));
                }

                res
            }
            ast::BareExpression::Whitespace => {
                if let Some(rule) = rule {
                    format!(
                        r#"self.builtin_whitespace(pos, input, None, None)
                        .map(|node| {{ Node {{
                            rule: Rule::{},
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            label: {},
                            alternative: {},
                        }}
                    }})"#,
                        rule,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                } else {
                    format!(
                        r#"self.builtin_whitespace(pos, input, {}, {})"#,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                }
            }
            ast::BareExpression::EndOfInput => {
                if let Some(rule) = rule {
                    format!(
                        r#"self.builtin_eoi(pos, input, None)
                        .map(|node| {{ Node {{
                            rule: Rule::{},
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            label: {},
                            alternative: {},
                        }}
                    }})"#,
                        rule,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                } else {
                    format!(
                        r#"self.builtin_eoi(pos, input, {}, {})"#,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                }
            }
            ast::BareExpression::Dot => {
                if let Some(rule) = rule {
                    format!(
                        r#"self.builtin_dot(pos, input, None, None)
                        .map(|node| {{ Node {{
                            rule: Rule::{},
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            label: {},
                            alternative: {},
                        }}
                    }})"#,
                        rule,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                } else {
                    format!(
                        r#"self.builtin_dot(pos, input, {}, {})"#,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                }
            }
            ast::BareExpression::XidIdentifier => {
                if let Some(rule) = rule {
                    format!(
                        r#"self.builtin_xid_identifier(pos, input, None, None)
                        .map(|node| {{ Node {{
                            rule: Rule::{},
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            label: {},
                            alternative: {},
                        }}
                    }})"#,
                        rule,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                } else {
                    format!(
                        r#"self.builtin_xid_identifier(pos, input, {}, {})"#,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                }
            }
            ast::BareExpression::StringLiteral(_) | ast::BareExpression::Regex(_) => {
                let terminal = self.terminals.get(expr).unwrap();

                format!(
                    r#"self.match_terminal(pos, input, Terminal::{})
                    .map(|end| Node::new(Rule::{}, pos, end, {}, {}))
                    .ok_or(pos)"#,
                    terminal,
                    rule.unwrap_or("Terminal"),
                    option_stringify(&expr.label),
                    option_stringify(alt),
                )
            }
            ast::BareExpression::Alternatives(list) => {
                let mut iter = list.iter();

                let first = iter.next().unwrap();

                let mut res = self.emit_expr(first, rule, &first.alt, grammar);

                for expr in iter {
                    res.push_str(&format!(
                        r#"
            .or_else(|_| {})"#,
                        self.emit_expr(expr, rule, &expr.alt, grammar),
                    ));
                }

                res
            }
            ast::BareExpression::Definition(rule_no) => {
                if let Some(rule) = rule {
                    format!(
                        r#"self.rule_{}(pos, input)
                        .map(|node| {{ Node {{
                            rule: Rule::{},
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            label: {},
                            alternative: {},
                        }}
                    }})"#,
                        grammar.definitions[*rule_no].name,
                        rule,
                        option_stringify(&expr.label),
                        option_stringify(alt),
                    )
                } else {
                    format!(
                        r#"self.rule_{}(pos, input)"#,
                        grammar.definitions[*rule_no].name
                    )
                }
            }
            ast::BareExpression::MemoDefinition(rule_no) => {
                format!(
                    r#"match self.rule_memo.get(&(pos, Rule::{})) {{
                    Some(e) => e.clone(),
                    None => Err(pos),
                }}"#,
                    grammar.definitions[*rule_no].name
                )
            }
            ast::BareExpression::Optional(expr) => {
                format!(
                    r#"{}.or_else(|_| Ok(Node::new(Rule::Terminal, pos, pos, {}, {})))"#,
                    self.emit_expr(expr, rule, alt, grammar),
                    option_stringify(&expr.label),
                    option_stringify(alt),
                )
            }
            ast::BareExpression::MustMatch(expr) => {
                format!(
                    r#"{}.map(|mut node| {{ node.end = node.start; node }})"#,
                    self.emit_expr(expr, rule, alt, grammar)
                )
            }
            ast::BareExpression::MustNotMatch(expr) => {
                format!(
                    r#"match {} {{
                    Ok(_) => Err(pos),
                    Err(_) => Ok(Node::new(Rule::{}, pos, pos, {}, {})),
                }}"#,
                    self.emit_expr(expr, rule, &None, grammar),
                    rule.unwrap_or("MustNotMatch"),
                    option_stringify(&expr.label),
                    option_stringify(alt),
                )
            }
            ast::BareExpression::Any(expr) => {
                format!(
                    r#"{{
                    let mut list = Vec::new();
                    let start = pos;
                    let mut pos = pos;

                    while let Ok(node) = {} {{
                        if pos == node.end {{
                            // must be making progress
                            break;
                        }}
                        pos = node.end;
                        list.push(node);
                    }}

                    Ok(Node {{
                        rule: Rule::{},
                        start,
                        end: pos,
                        children: list,
                        label: {},
                        alternative: {},
                    }})
                }}"#,
                    self.emit_expr(expr, rule, &None, grammar),
                    rule.unwrap_or("Any"),
                    option_stringify(&expr.label),
                    option_stringify(alt),
                )
            }
            ast::BareExpression::More(expr) => {
                format!(
                    r#"{{
                    let mut list = Vec::new();
                    let start = pos;
                    let mut pos = pos;

                    while let Ok(node) = {} {{
                        if pos == node.end {{
                            // must be making progress
                            break;
                        }}
                        pos = node.end;
                        list.push(node);
                    }}

                    if list.is_empty() {{
                        Err(start)
                    }} else {{
                        Ok(Node {{
                            rule: Rule::{},
                            start,
                            end: pos,
                            children: list,
                            label: {},
                            alternative: {},
                        }})
                    }}
                }}"#,
                    self.emit_expr(expr, rule, &None, grammar),
                    rule.unwrap_or("More"),
                    option_stringify(&expr.label),
                    option_stringify(alt),
                )
            }
        }
    }

    fn emit(&self, grammar: &ast::Grammar, modname: &str) -> String {
        let mut res = format!("mod {} {{", modname);
        res.push_str(
            r#"
#![allow(unused_imports, dead_code, clippy::all)]
use std::collections::HashMap;
use regex::Regex;
use unicode_xid::UnicodeXID;

#[derive(Clone, Debug)]
pub struct Node {
    pub rule: Rule,
    pub start: usize,
    pub end: usize,
    pub children: Vec<Node>,
    pub label: Option<&'static str>,
    pub alternative: Option<&'static str>,
}

impl Node {
    fn new(rule: Rule, start: usize, end: usize, label: Option<&'static str>, alternative: Option<&'static str>) -> Self {
        Self {
            rule,
            start,
            end,
            children: Vec::new(),
            label,
            alternative,
        }
    }

    pub fn print_to_string(&self, input: &str) -> String {
        let children = if self.children.is_empty() {
            String::new()
        } else {
            format!(", {}",
                self
                    .children
                    .iter()
                    .map(|node| node.print_to_string(input))
                    .collect::<Vec<String>>()
                    .join(", "))
        };

        format!(
            "({:?}, {}\"{}\"{})",
            self.rule,
            if let Some(alt) = self.alternative {
                format!("alt={}, ", alt)
            } else {
                String::new()
            },
            escape_string(&input[self.start..self.end]),
            children,
        )
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn as_str<'s>(&self, input: &'s str) -> &'s str {
        &input[self.start..self.end]
    }
}

fn escape_string(str: &str) -> String {
    let mut res = String::new();

    for ch in str.chars() {
        match ch {
            '\\' => res.push_str("\\\\"),
            '\t' => res.push_str("\\t"),
            '\n' => res.push_str("\\n"),
            '\r' => res.push_str("\\r"),
            '"' => res.push_str("\\\""),
            ch => res.push(ch),
        }
    }

    res
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
#[allow(non_camel_case_types)]
enum Terminal {
"#,
        );

        for f in self.terminals.values() {
            res.push_str(&format!("    {},\n", f));
        }

        res.push_str(
            r#"}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Rule {
    Terminal,
    List,
    MustNotMatch,
    Any,
    More,
"#,
        );

        for def in &grammar.definitions {
            res.push_str(&format!("    {},\n", def.name));
        }

        for name in self.builtins.values() {
            res.push_str(&format!("    {},\n", name));
        }

        res.push_str(
            r#"}

#[allow(non_snake_case)]
pub struct PEG {
    terminal_memo: HashMap<(usize, Terminal), Option<usize>>,
    rule_memo: HashMap<(usize, Rule), Result<Node, usize>>,"#,
        );

        for (expr, name) in &self.terminals {
            if let ast::BareExpression::Regex(_) = &expr.expr {
                res.push_str(&format!(
                    r#"
    regex_{}: Regex,"#,
                    name
                ));
            }
        }

        res.push_str(
            r#"
}

impl PEG {
    pub fn new() -> Self {
        Self {
            terminal_memo: HashMap::new(),
            rule_memo: HashMap::new(),"#,
        );

        for (expr, name) in &self.terminals {
            if let ast::BareExpression::Regex(r) = &expr.expr {
                assert!(!r.starts_with('^'), "regex {} should not start with ^", r);

                res.push_str(&format!(
                    r################"
            regex_{}: Regex::new(r########"^{}"########).unwrap(),"################,
                    name, r,
                ));
            }
        }

        res.push_str(
            r#"
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<Node, (usize, usize)> {
        self.rule_memo.clear();
        self.terminal_memo.clear();

        "#,
        );

        res.push_str(&format!(
            r#"self.rule_{}(0, input)
        "#,
            &grammar.definitions[0].name,
        ));

        res.push_str(
            r#".map_err(|pos| {
                let mut line_no = 1;
                let mut col_no = pos;

                for l in input
                    .char_indices()
                    .filter_map(|(index, c)| if c == '\n' { Some(index + 1) } else { None })
                {
                    if pos < l {
                        break;
                    }

                    if pos == l {
                        // chomp off new line
                        col_no -= 1;
                        break;
                    }

                    col_no = pos - l;

                    line_no += 1;
                }

                (line_no, col_no)
            })
        }"#,
        );

        for (rule_no, def) in grammar.definitions.iter().enumerate() {
            if Generator::is_rule_direct_left_recursive(
                rule_no,
                &def.sequence,
                &mut Vec::new(),
                grammar,
            ) {
                res.push_str(&self.emit_left_recursive_rule(rule_no, def, grammar));
            } else {
                res.push_str(&self.emit_rule(def, grammar));
            }
        }

        if self
            .builtins
            .iter()
            .any(|(key, _)| key.expr == ast::BareExpression::Whitespace)
        {
            res.push_str(
                r#"

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
    }"#,
            );
        }

        if self
            .builtins
            .iter()
            .any(|(key, _)| key.expr == ast::BareExpression::EndOfInput)
        {
            res.push_str(
                r#"

    fn builtin_eoi(&self, pos: usize, input: &str, label: Option<&'static str>, alternative: Option<&'static str>) -> Result<Node, usize> {
        // not worth caching
        if pos == input.len() {
            Ok(Node::new(Rule::EOI, pos, pos, label, alternative))
        } else {
            Err(pos)
        }
    }"#,
            );
        }

        if self
            .builtins
            .iter()
            .any(|(key, _)| key.expr == ast::BareExpression::Dot)
        {
            res.push_str(
                r#"

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
    }"#,
            );
        }

        if self
            .builtins
            .iter()
            .any(|(key, _)| key.expr == ast::BareExpression::XidIdentifier)
        {
            res.push_str(
                r#"

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
    }"#,
            );
        }

        res.push_str(
            r#"

    fn match_terminal(&mut self, pos: usize, input: &str, terminal: Terminal) -> Option<usize> {
        let key = (pos, terminal);

        if let Some(res) = self.terminal_memo.get(&key) {
            return *res;
        }

        let res = if pos > input.len() {
            None
        } else {
            match terminal {"#,
        );

        for (expr, name) in &self.terminals {
            match &expr.expr {
                ast::BareExpression::StringLiteral(s) if s.chars().count() == 1 => {
                    let ch = escape_char(s.chars().next().unwrap());
                    res.push_str(&format!(
                        r#"
                Terminal::{} => {{
                    if input[pos..].starts_with({}) {{
                        Some(pos + "{}".len())
                    }} else {{
                        None
                    }}
                }}"#,
                        name,
                        ch,
                        escape_string(s),
                    ));
                }
                ast::BareExpression::StringLiteral(s) => {
                    let s = escape_string(s);
                    res.push_str(&format!(
                        r#"
                Terminal::{} => {{
                    if input[pos..].starts_with("{}") {{
                        Some(pos + "{}".len())
                    }} else {{
                        None
                    }}
                }}"#,
                        name, s, s
                    ));
                }
                ast::BareExpression::Regex(_) => {
                    res.push_str(&format!(
                        r#"
                Terminal::{} => {{
                    self.regex_{}.find(&input[pos..]).map(|m| {{
                        m.end() + pos
                    }})
                }}"#,
                        name, name
                    ));
                }
                _ => (),
            }
        }

        res.push_str(
            r#"
            }
        };

        // Note that failure to match is also cached using None
        self.terminal_memo.insert(key, res);

        res
    }
}
}
"#,
        );

        res
    }
}

fn option_stringify(o: &Option<String>) -> String {
    match o {
        None => String::from("None"),
        Some(s) => format!(r#"Some("{}")"#, s),
    }
}

/// This function is supposed to called from a crate build.rs. You pass in the
/// directory where your .peg files reside ("src/" for example), and for each of
/// those, an .rs file with the generated parser will be created in the out
/// directory. You generally want this to be in cargo's OUT_DIR.
pub fn process_files(dir: &Path, out: &Path) {
    for entry in fs::read_dir(dir).expect("cannot read directory") {
        let entry = entry.expect("cannot read file");
        let path = entry.path();
        if path.is_dir() {
            process_files(&path, out);
        } else if path.is_file() && path.extension() == Some(OsStr::new("peg")) {
            let src = fs::read_to_string(&path).expect("failed to read input");

            let mut gen = Generator::new();

            let res = gen.build(&src, path.file_stem().unwrap().to_str().unwrap());

            let new_path = out.join(path.with_extension("rs").file_name().unwrap());

            fs::write(new_path, res).expect("failed to write generated parser")
        }
    }
}
