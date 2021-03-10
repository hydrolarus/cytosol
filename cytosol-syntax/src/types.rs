pub type FileId = usize;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FC {
    pub file: FileId,
    // as byte offsets in file
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Default, Clone)]
pub struct File {
    pub records: Vec<Record>,
    pub genes: Vec<Gene>,
    pub rules: Vec<Rule>,
    pub externs: Vec<Extern>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Identifier(pub FC, pub String);

#[derive(Debug, Clone)]
pub struct Record {
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
pub enum BindingAttribute {
    Quantity(FC, usize),
    Name(Identifier),
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub fc: FC,
    pub attr: Option<BindingAttribute>,
    pub name: Identifier,
}

#[derive(Debug, Clone)]
pub enum Type {
    Named(Identifier),
}

#[derive(Debug, Clone)]
pub struct Gene {
    pub fc: FC,
    pub factors: Vec<Binding>,
    pub when: Option<Expression>,
    pub body: Vec<GeneStatement>,
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub fc: FC,
    pub reactants: Vec<Binding>,
    pub when: Option<Expression>,
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
    Concentration(Identifier),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(FC, bool),
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

    Mul,
    Div,

    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,

    And,
    Or,
}
