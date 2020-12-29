pub type FileId = usize;

#[derive(Debug, Copy, Clone)]
pub struct FC {
    pub file: FileId,
    // as byte offsets in file
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Default, Clone)]
pub struct File {
    pub atoms: Vec<Atom>,
    pub genes: Vec<Gene>,
    pub enzymes: Vec<Enzyme>,
    pub externs: Vec<Extern>,
}

#[derive(Debug, Clone)]
pub struct Identifier(pub FC, pub String);

#[derive(Debug, Clone)]
pub struct Atom {
    pub fc: FC,
    pub name: Identifier,
    pub fields: Vec<(Identifier, Type)>,
}

#[derive(Debug, Clone)]
pub struct Extern {
    pub fc: FC,
    pub name: Identifier,
    pub parameters: Vec<(Identifier, Type)>,
}

#[derive(Debug, Clone)]
pub enum AtomBindingAttribute {
    Quantity(FC, usize),
    Name(Identifier),
}

#[derive(Debug, Clone)]
pub struct AtomBinding {
    pub fc: FC,
    pub attr: Option<AtomBindingAttribute>,
    pub name: Identifier,
}

#[derive(Debug, Clone)]
pub enum Type {
    Named(Identifier),
}

#[derive(Debug, Clone)]
pub struct Gene {
    pub fc: FC,
    pub factors: Vec<AtomBinding>,
    pub body: Vec<GeneStatement>,
}

#[derive(Debug, Clone)]
pub struct Enzyme {
    pub fc: FC,
    pub name: Identifier,
    pub reactants: Vec<AtomBinding>,
    pub products: Vec<Product>,
}

#[derive(Debug, Clone)]
pub struct Product {
    pub fc: FC,
    pub quantity: Option<(FC, usize)>,
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
    Express(FC, Product),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(Identifier),
    FieldAccess {
        base: Box<Expression>,
        field_name: Identifier,
    },
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
