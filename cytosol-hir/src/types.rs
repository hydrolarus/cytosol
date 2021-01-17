use cytosol_syntax::{Identifier, InfixOperator, PrefixOperator};
use id_arena::Id;

pub type TypeId = Id<Type>;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    Record(RecordId),
}

pub type RecordId = Id<Record>;

#[derive(Debug, Clone)]
pub struct Record {
    pub name: Identifier,
    pub field_names: Vec<Identifier>,
    pub fields: Vec<TypeId>,
}

pub type FieldIndex = usize;

#[derive(Debug)]
pub struct Product {
    pub quantity: usize,
    pub record: RecordId,
    pub arguments: Vec<ExpressionId>,
}

#[derive(Debug)]
pub enum Bind {
    None,
    Quantity(usize),
    Named(Identifier),
}

pub type RuleId = Id<Rule>;

#[derive(Debug)]
pub struct Rule {
    pub binds: Vec<(Bind, RecordId)>,
    pub products: Vec<Product>,
}

pub type ExternId = Id<Extern>;

#[derive(Debug, Clone)]
pub struct Extern {
    pub name: Identifier,
    pub parameter_names: Vec<Identifier>,
    pub parameters: Vec<TypeId>,
}

pub type GeneId = Id<Gene>;

#[derive(Debug)]
pub struct Gene {
    pub binds: Vec<(Bind, RecordId)>,
    pub body: Vec<GeneStatementId>,
}

pub type GeneStatementId = Id<GeneStatement>;

#[derive(Debug)]
pub enum GeneStatement {
    Call {
        ext: ExternId,
        arguments: Vec<ExpressionId>,
    },
    Express(Product),
}

pub type ExpressionId = Id<Expression>;

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(usize),
    StringLiteral(String),
    Variable(Identifier),
    FieldAccess {
        base: ExpressionId,
        field: FieldIndex,
    },
    PrefixOp {
        op: PrefixOperator,
        expr: ExpressionId,
    },
    InfixOp {
        op: InfixOperator,
        args: [ExpressionId; 2],
    },
}
