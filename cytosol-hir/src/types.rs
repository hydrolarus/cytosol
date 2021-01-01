use cytosol_syntax::{Identifier, InfixOperator, PrefixOperator};
use id_arena::Id;

pub type TypeId = Id<Type>;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    Atom(AtomId),
    Enzyme(EnzymeId),
}

pub type AtomId = Id<Atom>;

#[derive(Debug, Clone)]
pub struct Atom {
    pub name: Identifier,
    pub field_names: Vec<Identifier>,
    pub fields: Vec<TypeId>,
}

pub type FieldIndex = usize;

#[derive(Debug)]
pub enum Product {
    Enzyme {
        quantity: usize,
        enzyme: EnzymeId,
    },
    Atom {
        quantity: usize,
        atom: AtomId,
        arguments: Vec<ExpressionId>,
    },
}

#[derive(Debug)]
pub enum Bind {
    None,
    Quantity(usize),
    Named(Identifier),
}

#[derive(Debug)]
pub enum BindType {
    Atom(AtomId),
    Enzyme(EnzymeId),
}

pub type EnzymeId = Id<Enzyme>;

#[derive(Debug)]
pub struct Enzyme {
    pub name: Identifier,
    pub binds: Vec<(Bind, BindType)>,
    pub products: Vec<Product>,
}

pub type ExternId = Id<Extern>;

#[derive(Debug)]
pub struct Extern {
    pub name: Identifier,
    pub parameters: Vec<TypeId>,
}

pub type GeneId = Id<Gene>;

#[derive(Debug)]
pub struct Gene {
    pub binds: Vec<(Bind, BindType)>,
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
