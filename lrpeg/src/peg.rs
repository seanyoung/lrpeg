mod peg {
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
    pub alternative: Option<u16>
}

impl Node {
    fn new(rule: Rule, start: usize, end: usize, alternative: Option<u16>) -> Self {
        Self {
            rule,
            start,
            end,
            children: Vec::new(),
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
    Literal_8,
    Literal_7,
    Literal_5,
    Literal_6,
    Literal_3,
    Literal_4,
    Literal_9,
    Literal_1,
    Literal_0,
    Literal,
    Literal_2,
    Regex,
    Regex_0,
    Regex_1,
    re,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Rule {
    Terminal,
    List,
    MustNotMatch,
    Any,
    More,
    grammar,
    definition,
    expression,
    sequence,
    alternative,
    primary,
    string_literal,
    regex,
    ws,
    COMMENT,
    Dot,
    WHITESPACE,
    EOI,
    XID_IDENTIFIER,
}

#[allow(non_snake_case)]
pub struct PEG {
    terminal_memo: HashMap<(usize, Terminal), Option<usize>>,
    rule_memo: HashMap<(usize, Rule), Result<Node, usize>>,
    regex_Regex: Regex,
    regex_Regex_0: Regex,
    regex_Regex_1: Regex,
    regex_re: Regex,
}

impl PEG {
    pub fn new() -> Self {
        Self {
            terminal_memo: HashMap::new(),
            rule_memo: HashMap::new(),
            regex_Regex: Regex::new(r########"^"([^"\\]|\\.)*""########).unwrap(),
            regex_Regex_0: Regex::new(r########"^'([^'\\]|\\.)*'"########).unwrap(),
            regex_Regex_1: Regex::new(r########"^//[^
]*"########).unwrap(),
            regex_re: Regex::new(r########"^re#([^#\\]|\\.)*#"########).unwrap(),
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<Node, (usize, usize)> {
        self.rule_memo.clear();
        self.terminal_memo.clear();

        self.rule_grammar(0, input)
        .map_err(|pos| {
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
        }

    #[allow(non_snake_case)]
    fn rule_grammar(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::grammar);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
            let mut list = Vec::new();
            let start = pos;

            self.rule_ws(pos, input)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                {
                    let mut list = Vec::new();
                    let start = pos;
                    let mut pos = pos;

                    while let Ok(node) = self.rule_definition(pos, input) {
                        if pos == node.end {
                            // must be making progress
                            break;
                        }
                        pos = node.end;
                        list.push(node);
                    }

                    if list.is_empty() {
                        Err(start)
                    } else {
                        Ok(Node {
                            rule: Rule::More,
                            start,
                            end: pos,
                            children: list,
                            alternative: None,
                        })
                    }
                }
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.builtin_eoi(pos, input, None)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::grammar,
                    start,
                    end,
                    children: list,
                    alternative: None,
                }
            })
        };

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_definition(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::definition);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
            let mut list = Vec::new();
            let start = pos;

            self.builtin_xid_identifier(pos, input, None)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.match_terminal(pos, input, Terminal::Literal)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_expression(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.match_terminal(pos, input, Terminal::Literal_0)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos).or_else(|_| Ok(Node::new(Rule::Terminal, pos, pos, None)))
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::definition,
                    start,
                    end,
                    children: list,
                    alternative: None,
                }
            })
        };

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_expression(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::expression);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
            let mut list = Vec::new();
            let start = pos;

            self.rule_sequence(pos, input)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                {
                    let mut list = Vec::new();
                    let start = pos;
                    let mut pos = pos;

                    while let Ok(node) = {
            let mut list = Vec::new();
            let start = pos;

            self.match_terminal(pos, input, Terminal::Literal_1)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_sequence(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::List,
                    start,
                    end,
                    children: list,
                    alternative: None,
                }
            })
        } {
                        if pos == node.end {
                            // must be making progress
                            break;
                        }
                        pos = node.end;
                        list.push(node);
                    }

                    Ok(Node {
                        rule: Rule::Any,
                        start,
                        end: pos,
                        children: list,
                        alternative: None,
                    })
                }
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::expression,
                    start,
                    end,
                    children: list,
                    alternative: None,
                }
            })
        };

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_sequence(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::sequence);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
                    let mut list = Vec::new();
                    let start = pos;
                    let mut pos = pos;

                    while let Ok(node) = self.rule_alternative(pos, input)
                        .map(|node| { Node {
                            rule: Rule::sequence,
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            alternative: None,
                        }
                    }) {
                        if pos == node.end {
                            // must be making progress
                            break;
                        }
                        pos = node.end;
                        list.push(node);
                    }

                    if list.is_empty() {
                        Err(start)
                    } else {
                        Ok(Node {
                            rule: Rule::sequence,
                            start,
                            end: pos,
                            children: list,
                            alternative: None,
                        })
                    }
                };

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_alternative(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::alternative);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        self.rule_memo.insert(key, Err(pos));

        let mut res = Err(pos);
        let mut next_pos = pos;

        loop {
            let r = {
            let mut list = Vec::new();
            let start = pos;

            match self.rule_memo.get(&(pos, Rule::alternative)) {
                    Some(e) => e.clone(),
                    None => Err(pos),
                }
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.match_terminal(pos, input, Terminal::Literal_2)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::alternative,
                    start,
                    end,
                    children: list,
                    alternative: Some(0),
                }
            })
        }
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            match self.rule_memo.get(&(pos, Rule::alternative)) {
                    Some(e) => e.clone(),
                    None => Err(pos),
                }
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.match_terminal(pos, input, Terminal::Literal_3)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::alternative,
                    start,
                    end,
                    children: list,
                    alternative: Some(1),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            match self.rule_memo.get(&(pos, Rule::alternative)) {
                    Some(e) => e.clone(),
                    None => Err(pos),
                }
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.match_terminal(pos, input, Terminal::Literal_4)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::alternative,
                    start,
                    end,
                    children: list,
                    alternative: Some(2),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.match_terminal(pos, input, Terminal::Literal_5)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_expression(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.match_terminal(pos, input, Terminal::Literal_6)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::alternative,
                    start,
                    end,
                    children: list,
                    alternative: Some(3),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.rule_primary(pos, input)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::alternative,
                    start,
                    end,
                    children: list,
                    alternative: Some(4),
                }
            })
        });

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
    }

    #[allow(non_snake_case)]
    fn rule_primary(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::primary);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
            let mut list = Vec::new();
            let start = pos;

            self.match_terminal(pos, input, Terminal::Literal_7)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_primary(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::primary,
                    start,
                    end,
                    children: list,
                    alternative: Some(0),
                }
            })
        }
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.match_terminal(pos, input, Terminal::Literal_8)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_primary(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::primary,
                    start,
                    end,
                    children: list,
                    alternative: Some(1),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.rule_regex(pos, input)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::primary,
                    start,
                    end,
                    children: list,
                    alternative: Some(2),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.builtin_xid_identifier(pos, input, None)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                match self.match_terminal(pos, input, Terminal::Literal)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos) {
                    Ok(_) => Err(pos),
                    Err(_) => Ok(Node::new(Rule::MustNotMatch, pos, pos, None)),
                }
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::primary,
                    start,
                    end,
                    children: list,
                    alternative: Some(3),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.rule_string_literal(pos, input)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::primary,
                    start,
                    end,
                    children: list,
                    alternative: Some(4),
                }
            })
        })
            .or_else(|_| {
            let mut list = Vec::new();
            let start = pos;

            self.match_terminal(pos, input, Terminal::Literal_9)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.rule_ws(pos, input)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::primary,
                    start,
                    end,
                    children: list,
                    alternative: Some(5),
                }
            })
        });

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_string_literal(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::string_literal);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = self.match_terminal(pos, input, Terminal::Regex)
                    .map(|end| Node::new(Rule::string_literal, pos, end, Some(0)))
                    .ok_or(pos)
            .or_else(|_| self.match_terminal(pos, input, Terminal::Regex_0)
                    .map(|end| Node::new(Rule::string_literal, pos, end, Some(1)))
                    .ok_or(pos));

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_regex(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::regex);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = self.match_terminal(pos, input, Terminal::re)
                    .map(|end| Node::new(Rule::regex, pos, end, None))
                    .ok_or(pos);

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_ws(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::ws);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
                    let mut list = Vec::new();
                    let start = pos;
                    let mut pos = pos;

                    while let Ok(node) = self.rule_COMMENT(pos, input)
                        .map(|node| { Node {
                            rule: Rule::ws,
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            alternative: Some(0),
                        }
                    })
            .or_else(|_| self.builtin_whitespace(pos, input, None)
                        .map(|node| { Node {
                            rule: Rule::ws,
                            start: node.start,
                            end: node.end,
                            children: vec![node],
                            alternative: Some(1),
                        }
                    })) {
                        if pos == node.end {
                            // must be making progress
                            break;
                        }
                        pos = node.end;
                        list.push(node);
                    }

                    Ok(Node {
                        rule: Rule::ws,
                        start,
                        end: pos,
                        children: list,
                        alternative: None,
                    })
                };

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    #[allow(non_snake_case)]
    fn rule_COMMENT(&mut self, pos: usize, input: &str) -> Result<Node, usize> {
        let key = (pos, Rule::COMMENT);

        if let Some(res) = self.rule_memo.get(&key) {
            return res.clone();
        }

        let res = {
            let mut list = Vec::new();
            let start = pos;

            self.match_terminal(pos, input, Terminal::Regex_1)
                    .map(|end| Node::new(Rule::Terminal, pos, end, None))
                    .ok_or(pos)
            .and_then(|node| {
                let pos = node.end;
                list.push(node);
                self.builtin_dot(pos, input, None)
            })
            .map(|node| {
                let end = node.end;
                list.push(node);

                Node {
                    rule: Rule::COMMENT,
                    start,
                    end,
                    children: list,
                    alternative: None,
                }
            })
        };

        // Note that failure to match is also cached using Err()
        self.rule_memo.insert(key, res.clone());

        res
    }

    fn builtin_whitespace(&self, pos: usize, input: &str, alt: Option<u16>) -> Result<Node, usize> {
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

        Ok(Node::new(Rule::WHITESPACE, pos, next_pos, alt))
    }

    fn builtin_eoi(&self, pos: usize, input: &str, alt: Option<u16>) -> Result<Node, usize> {
        // not worth caching
        if pos == input.len() {
            Ok(Node::new(Rule::EOI, pos, pos, alt))
        } else {
            Err(pos)
        }
    }

    fn builtin_dot(&self, pos: usize, input: &str, alt: Option<u16>) -> Result<Node, usize> {
        // not worth caching
        let mut chars = input[pos..].char_indices();
        if chars.next().is_some() {
            if let Some((len, _)) = chars.next() {
                Ok(Node::new(Rule::Dot, pos, pos + len, alt))
            } else {
                Ok(Node::new(Rule::Dot, pos, input.len(), alt))
            }
        } else {
            Err(pos)
        }
    }

    fn builtin_xid_identifier(&mut self, pos: usize, input: &str, alt: Option<u16>) -> Result<Node, usize> {
        let key = (pos, Rule::XID_IDENTIFIER);

        if let Some(res) = self.rule_memo.get(&key) {{
            let mut res = res.clone();
            if let Ok(res) = &mut res {
                res.alternative = alt;
            }
            return res;
        }}

        let mut chars = input[pos..].char_indices();
        let mut end = pos;
        let mut res = if let Some((_, ch)) = chars.next() {
            if UnicodeXID::is_xid_start(ch) {
                while {
                    if let Some((off, ch)) = chars.next() {
                        end = pos + off;
                        UnicodeXID::is_xid_continue(ch)
                    } else {
                        false
                    }
                } {}

                Ok(Node::new(Rule::XID_IDENTIFIER, pos, end, alt))
            } else {
                Err(pos)
            }
        } else {
            Err(pos)
        };

        self.rule_memo.insert(key, res.clone());

        if let Ok(res) = &mut res {
            res.alternative = alt;
        }

        res
    }

    fn match_terminal(&mut self, pos: usize, input: &str, terminal: Terminal) -> Option<usize> {
        let key = (pos, terminal);

        if let Some(res) = self.terminal_memo.get(&key) {
            return *res;
        }

        let res = if pos > input.len() {
            None
        } else {
            match terminal {
                Terminal::Literal_8 => {
                    if input[pos..].starts_with('!') {
                        Some(pos + "!".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_7 => {
                    if input[pos..].starts_with('&') {
                        Some(pos + "&".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_5 => {
                    if input[pos..].starts_with('(') {
                        Some(pos + "(".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_6 => {
                    if input[pos..].starts_with(')') {
                        Some(pos + ")".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_3 => {
                    if input[pos..].starts_with('*') {
                        Some(pos + "*".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_4 => {
                    if input[pos..].starts_with('+') {
                        Some(pos + "+".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_9 => {
                    if input[pos..].starts_with('.') {
                        Some(pos + ".".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_1 => {
                    if input[pos..].starts_with('/') {
                        Some(pos + "/".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_0 => {
                    if input[pos..].starts_with(';') {
                        Some(pos + ";".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal => {
                    if input[pos..].starts_with("<-") {
                        Some(pos + "<-".len())
                    } else {
                        None
                    }
                }
                Terminal::Literal_2 => {
                    if input[pos..].starts_with('?') {
                        Some(pos + "?".len())
                    } else {
                        None
                    }
                }
                Terminal::Regex => {
                    self.regex_Regex.find(&input[pos..]).map(|m| {
                        m.end() + pos
                    })
                }
                Terminal::Regex_0 => {
                    self.regex_Regex_0.find(&input[pos..]).map(|m| {
                        m.end() + pos
                    })
                }
                Terminal::Regex_1 => {
                    self.regex_Regex_1.find(&input[pos..]).map(|m| {
                        m.end() + pos
                    })
                }
                Terminal::re => {
                    self.regex_re.find(&input[pos..]).map(|m| {
                        m.end() + pos
                    })
                }
            }
        };

        // Note that failure to match is also cached using None
        self.terminal_memo.insert(key, res);

        res
    }
}
}

