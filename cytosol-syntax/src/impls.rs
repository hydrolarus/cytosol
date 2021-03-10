use std::{borrow::Borrow, ops::Range};

use crate::types::{
    Binding, Expression, Gene, GeneStatement, Identifier, Literal, Product, Record, Rule, Type, FC,
};

impl FC {
    pub fn merge(&self, other: impl Borrow<FC>) -> FC {
        let other = other.borrow();

        debug_assert_eq!(self.file, other.file);

        FC {
            file: self.file,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}

pub trait HasFC {
    fn fc(&self) -> FC;
}

impl HasFC for Identifier {
    fn fc(&self) -> FC {
        self.0
    }
}

impl HasFC for Record {
    fn fc(&self) -> FC {
        self.fc
    }
}

impl HasFC for Binding {
    fn fc(&self) -> FC {
        self.fc
    }
}

impl HasFC for Type {
    fn fc(&self) -> FC {
        match self {
            Type::Named(n) => n.fc(),
        }
    }
}

impl HasFC for Gene {
    fn fc(&self) -> FC {
        self.fc
    }
}

impl HasFC for Rule {
    fn fc(&self) -> FC {
        self.fc
    }
}
impl HasFC for Product {
    fn fc(&self) -> FC {
        self.fc
    }
}
impl HasFC for GeneStatement {
    fn fc(&self) -> FC {
        match self {
            GeneStatement::Call { fc, .. } => *fc,
            GeneStatement::Express(fc, _) => *fc,
        }
    }
}
impl HasFC for Expression {
    fn fc(&self) -> FC {
        match self {
            Expression::Literal(l) => l.fc(),
            Expression::Variable(id) => id.fc(),
            Expression::FieldAccess { base, field_name } => base.fc().merge(field_name.fc()),
            Expression::PrefixOp { op, expr } => op.0.merge(expr.fc()),
            Expression::InfixOp { op: _, args } => args[0].fc().merge(args[1].fc()),
            Expression::Concentration(ty) => ty.0,
        }
    }
}
impl HasFC for Literal {
    fn fc(&self) -> FC {
        match self {
            Literal::Bool(fc, _) => *fc,
            Literal::Integer(fc, _) => *fc,
            Literal::String(fc, _) => *fc,
        }
    }
}
