use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct FC {
    pub file: Option<Arc<str>>,
    // as byte offsets in file
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Identifier(pub FC, pub String);

#[derive(Debug, Clone)]
pub struct AtomBinding {
    pub fc: FC,
    pub name: Identifier,
    pub fields: Vec<(Identifier, Type)>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int(FC),
    String(FC),
}

#[derive(Debug, Clone)]
pub struct Gene {
    pub fc: FC,
    pub factors: Vec<Quantified<AtomBinding>>,
    pub body: Vec<GeneStatement>,
}

#[derive(Debug, Clone)]
pub struct Quantified<T> {
    pub quantity: Option<(FC, usize)>,
    pub value: T,
}

#[derive(Debug, Clone)]
pub struct Enzyme {
    pub fc: FC,
    pub name: Identifier,
    pub reactants: Vec<Quantified<AtomBinding>>,
    pub products: Vec<Quantified<Product>>,
}

#[derive(Debug, Clone)]
pub struct Product {
    pub fc: FC,
    pub name: Identifier,
    pub fields: Vec<(Identifier, Expression)>,
}

#[derive(Debug, Clone)]
pub enum GeneStatement {
    /// External function call
    Call {
        fc: FC,
        name: Identifier,
        arguments: Vec<(Identifier, Expression)>,
    },
    Express(FC, Quantified<Product>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(Identifier),
    PrefixOp {
        op: (FC, PrefixOperator),
        expr: Box<Expression>,
    },
    InfixOp {
        op: (FC, InfixOperator),
        args: Box<[Expression; 2]>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(FC, usize),
    String(FC, String),
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOperator {
    Neg,
}

#[derive(Debug, Clone, Copy)]
pub enum InfixOperator {
    Add,
    Sub,
}
