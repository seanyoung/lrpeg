use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Grammar {
    // FIXME: use &str rather than owned String
    pub lookup: HashMap<String, usize>,
    pub definitions: Vec<Definition>,
}

#[derive(Clone, Debug)]
pub struct Definition {
    pub name: String,
    pub sequence: Expression,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Expression {
    Dot,
    Definition(usize),
    MemoDefinition(usize),
    StringLiteral(String),
    Regex(String),
    MustMatch(Box<Expression>),
    MustNotMatch(Box<Expression>),
    Optional(Box<Expression>),
    Any(Box<Expression>),
    More(Box<Expression>),
    List(Vec<Expression>),
    Alternatives(Vec<Expression>),
}
