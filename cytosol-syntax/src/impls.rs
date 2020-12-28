use std::borrow::Borrow;

use crate::types::{
    AtomBinding, Enzyme, Expression, Gene, GeneStatement, Identifier, Literal, Product, Quantified,
    Type, FC,
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
}

pub trait HasFC {
    fn fc(&self) -> FC;
}

impl HasFC for Identifier {
    fn fc(&self) -> FC {
        self.0.clone()
    }
}
impl HasFC for AtomBinding {
    fn fc(&self) -> FC {
        self.fc.clone()
    }
}
impl HasFC for Type {
    fn fc(&self) -> FC {
        match self {
            Type::Int(fc) => fc.clone(),
            Type::String(fc) => fc.clone(),
        }
    }
}
impl HasFC for Gene {
    fn fc(&self) -> FC {
        self.fc.clone()
    }
}
impl<T: HasFC> HasFC for Quantified<T> {
    fn fc(&self) -> FC {
        if let Some((fc, _)) = &self.quantity {
            fc.merge(self.value.fc())
        } else {
            self.value.fc()
        }
    }
}
impl HasFC for Enzyme {
    fn fc(&self) -> FC {
        self.fc.clone()
    }
}
impl HasFC for Product {
    fn fc(&self) -> FC {
        self.fc.clone()
    }
}
impl HasFC for GeneStatement {
    fn fc(&self) -> FC {
        match self {
            GeneStatement::Call { fc, .. } => fc.clone(),
            GeneStatement::Express(fc, _) => fc.clone(),
        }
    }
}
impl HasFC for Expression {
    fn fc(&self) -> FC {
        match self {
            Expression::Literal(l) => l.fc(),
            Expression::Variable(id) => id.fc(),
            Expression::PrefixOp { op, expr } => op.0.merge(expr.fc()),
            Expression::InfixOp { op: _, args } => args[0].fc().merge(args[1].fc()),
        }
    }
}
impl HasFC for Literal {
    fn fc(&self) -> FC {
        match self {
            Literal::Integer(fc, _) => fc.clone(),
            Literal::String(fc, _) => fc.clone(),
        }
    }
}
