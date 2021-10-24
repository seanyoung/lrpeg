use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub struct Grammar {
    pub lookup: BTreeMap<String, usize>,
    pub definitions: Vec<Definition>,
}

#[derive(Clone, Debug)]
pub struct Definition {
    pub name: String,
    pub sequence: Expression,
}

#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Expression {
    pub label: Option<String>,
    pub alt: Option<String>,
    pub expr: BareExpression,
}

impl From<BareExpression> for Expression {
    fn from(expr: BareExpression) -> Self {
        Expression {
            label: None,
            alt: None,
            expr,
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum BareExpression {
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
