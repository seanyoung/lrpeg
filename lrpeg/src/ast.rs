use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub struct Grammar {
    // FIXME: use &str rather than owned String
    pub lookup: BTreeMap<String, usize>,
    pub definitions: Vec<Definition>,
}

#[derive(Clone, Debug)]
pub struct Definition {
    pub name: String,
    pub sequence: Expression,
}

#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Expression {
    Dot,
    Whitespace,
    EndOfInput,
    XidIdentifier, // unicode identifier
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
