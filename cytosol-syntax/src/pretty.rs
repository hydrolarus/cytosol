use pretty::RcDoc as Doc;

use crate::{
    Atom, AtomBinding, AtomBindingAttribute, Enzyme, Expression, Extern, File, Gene, GeneStatement,
    Identifier, InfixOperator, Literal, PrefixOperator, Product, Type,
};

pub fn pretty_print<T: ToDoc>(val: &T, width: usize) -> String {
    let doc = val.to_doc();
    let mut v = Vec::new();
    doc.render(width, &mut v).unwrap();
    String::from_utf8_lossy(&v).to_string()
}

pub trait ToDoc {
    fn to_doc(&self) -> Doc;
}

impl<T: ToDoc> ToDoc for Vec<T> {
    fn to_doc(&self) -> Doc {
        if self.is_empty() {
            Doc::text("()")
        } else {
            Doc::text("(")
                .append(Doc::line_().append(Doc::intersperse(
                    self.iter().map(|t| t.to_doc()),
                    Doc::line(),
                )))
                .nest(4)
                .group()
                .append(")")
        }
    }
}

impl ToDoc for File {
    fn to_doc(&self) -> Doc {
        Doc::text("(file")
            .append(
                Doc::line()
                    .append(self.atoms.to_doc())
                    .append(Doc::hardline())
                    .append(self.genes.to_doc())
                    .append(Doc::hardline())
                    .append(self.enzymes.to_doc().group())
                    .append(Doc::hardline())
                    .append(self.externs.to_doc().group())
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
    }
}
impl ToDoc for Identifier {
    fn to_doc(&self) -> Doc {
        Doc::text(&self.1)
    }
}
impl ToDoc for Extern {
    fn to_doc(&self) -> Doc {
        Doc::text("(extern")
            .append(
                Doc::line()
                    .append(self.name.to_doc())
                    .append(Doc::space())
                    .append(self.parameters.to_doc())
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
    }
}

impl ToDoc for Atom {
    fn to_doc(&self) -> Doc {
        Doc::text("(atom")
            .append(
                Doc::line()
                    .append(self.name.to_doc())
                    .append(Doc::space())
                    .append(self.fields.to_doc())
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
    }
}

impl ToDoc for AtomBinding {
    fn to_doc(&self) -> Doc {
        let attr = match &self.attr {
            Some(AtomBindingAttribute::Name(n)) => n.to_doc().append(Doc::space()),
            Some(AtomBindingAttribute::Quantity(_, n)) => Doc::as_string(n).append(Doc::space()),
            None => Doc::nil(),
        };

        Doc::text("(atom ")
            .append(attr)
            .append(self.name.to_doc())
            .append(Doc::text(")"))
            .group()
    }
}
impl ToDoc for Type {
    fn to_doc(&self) -> Doc {
        match self {
            Type::Named(n) => n.to_doc(),
        }
    }
}
impl ToDoc for Gene {
    fn to_doc(&self) -> Doc {
        Doc::text("(gene")
            .append(
                Doc::hardline()
                    .append(self.factors.to_doc())
                    .append(Doc::hardline())
                    .append(self.body.to_doc())
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
            .group()
    }
}

impl ToDoc for Enzyme {
    fn to_doc(&self) -> Doc {
        Doc::text("(enzyme")
            .append(
                Doc::line()
                    .append(self.name.to_doc())
                    .append(Doc::line())
                    .append(self.reactants.to_doc())
                    .append(Doc::line())
                    .append(self.products.to_doc())
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
            .group()
    }
}
impl ToDoc for Product {
    fn to_doc(&self) -> Doc {
        if self.fields.is_empty() {
            Doc::text("(product")
                .append(Doc::line())
                .append(self.name.to_doc())
                .append(")")
                .group()
        } else {
            Doc::text("(product")
                .append(Doc::line())
                .append(self.name.to_doc())
                .append(Doc::line())
                .append(self.fields.to_doc())
                .append(")")
                .group()
        }
    }
}
impl ToDoc for GeneStatement {
    fn to_doc(&self) -> Doc {
        match self {
            GeneStatement::Call {
                fc: _,
                name,
                arguments,
            } => Doc::text("(call")
                .append(Doc::line())
                .append(name.to_doc())
                .append(Doc::line())
                .append(arguments.to_doc())
                .append(")")
                .group(),
            GeneStatement::Express(_, prod) => Doc::text("(express")
                .append(Doc::line())
                .append(prod.to_doc())
                .append(")")
                .group(),
        }
    }
}
impl ToDoc for Expression {
    fn to_doc(&self) -> Doc {
        match self {
            Expression::Literal(l) => l.to_doc(),
            Expression::Variable(v) => v.to_doc(),
            Expression::FieldAccess { base, field_name } => Doc::text("(.")
                .append(field_name.to_doc())
                .append(Doc::line())
                .append(base.to_doc())
                .append(")")
                .group(),
            Expression::PrefixOp { op: (_, op), expr } => Doc::text("(")
                .append(op.to_doc())
                .append(Doc::line())
                .append(expr.to_doc())
                .append(")")
                .group(),
            Expression::InfixOp { op: (_, op), args } => Doc::text("(")
                .append(op.to_doc())
                .append(Doc::line())
                .append(args[0].to_doc())
                .append(Doc::line())
                .append(args[1].to_doc())
                .append(")")
                .group(),
        }
    }
}
impl ToDoc for Literal {
    fn to_doc(&self) -> Doc {
        match self {
            Literal::Integer(_, i) => Doc::as_string(i),
            Literal::String(_, s) => Doc::text(format!("{:?}", s)),
        }
    }
}
impl ToDoc for PrefixOperator {
    fn to_doc(&self) -> Doc {
        match self {
            PrefixOperator::Neg => Doc::text("-"),
        }
    }
}
impl ToDoc for InfixOperator {
    fn to_doc(&self) -> Doc {
        match self {
            InfixOperator::Add => Doc::text("+"),
            InfixOperator::Sub => Doc::text("-"),
        }
    }
}

impl ToDoc for (Identifier, Type) {
    fn to_doc(&self) -> Doc {
        Doc::text("(")
            .append(
                self.0
                    .to_doc()
                    .append(Doc::space())
                    .append(self.1.to_doc())
                    .group(),
            )
            .append(")")
            .group()
    }
}

impl ToDoc for (Identifier, Expression) {
    fn to_doc(&self) -> Doc {
        Doc::text("(")
            .append(
                self.0
                    .to_doc()
                    .append(Doc::space())
                    .append(self.1.to_doc())
                    .group(),
            )
            .append(")")
            .group()
    }
}
