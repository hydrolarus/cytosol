use pretty::RcDoc as Doc;

use cytosol::{
    parser::Token,
    syntax::{
        Binding, BindingAttribute, Expression, Extern, File, Gene, GeneStatement, Identifier,
        InfixOperator, Literal, PrefixOperator, Product, Record, Rule, Type,
    },
};

pub(crate) fn dump_tokens<'a>(toks: impl Iterator<Item = Token<'a>>) {
    for tok in toks {
        eprintln!("{:?}", tok.kind);
    }
}

pub(crate) fn dump_ast(ast: &File) {
    eprintln!("{}", pretty_print(ast, 80));
}

pub(crate) fn pretty_print<T: ToDoc>(val: &T, width: usize) -> String {
    let doc = val.to_doc();
    let mut v = Vec::new();
    doc.render(width, &mut v).unwrap();
    String::from_utf8_lossy(&v).to_string()
}

pub(crate) trait ToDoc {
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
                    .append(self.records.to_doc())
                    .append(Doc::hardline())
                    .append(self.genes.to_doc())
                    .append(Doc::hardline())
                    .append(self.rules.to_doc().group())
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

impl ToDoc for Record {
    fn to_doc(&self) -> Doc {
        Doc::text("(record")
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

impl ToDoc for Binding {
    fn to_doc(&self) -> Doc {
        let attr = match &self.attr {
            Some(BindingAttribute::Name(n)) => n.to_doc().append(Doc::space()),
            Some(BindingAttribute::Quantity(_, n)) => Doc::as_string(n).append(Doc::space()),
            None => Doc::nil(),
        };

        Doc::text("(record ")
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
        let when = if let Some(expr) = &self.when {
            Doc::text("(when ")
                .append(expr.to_doc())
                .append(")")
                .append(Doc::hardline())
                .group()
        } else {
            Doc::nil()
        };
        Doc::text("(gene")
            .append(
                Doc::hardline()
                    .append(self.factors.to_doc())
                    .append(Doc::hardline())
                    .append(when)
                    .append(self.body.to_doc())
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
            .group()
    }
}

impl ToDoc for Rule {
    fn to_doc(&self) -> Doc {
        let when = if let Some(expr) = &self.when {
            Doc::hardline()
                .append("(when ")
                .append(expr.to_doc())
                .append(")")
                .group()
        } else {
            Doc::nil()
        };
        Doc::text("(rule")
            .append(
                Doc::line()
                    .append(self.reactants.to_doc())
                    .append(Doc::line())
                    .append(self.products.to_doc())
                    .append(when)
                    .nest(4)
                    .group(),
            )
            .append(Doc::text(")"))
            .group()
    }
}
impl ToDoc for Product {
    fn to_doc(&self) -> Doc {
        let quantity = if let Some((_, n)) = &self.quantity {
            Doc::as_string(*n).append(Doc::line())
        } else {
            Doc::nil()
        };

        if self.fields.is_empty() {
            Doc::text("(product")
                .append(Doc::line())
                .append(quantity)
                .append(self.name.to_doc())
                .append(")")
                .group()
        } else {
            Doc::text("(product")
                .append(Doc::line())
                .append(quantity)
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
            Expression::Concentration(c) => Doc::text("(concentration")
                .append(Doc::line())
                .append(c.to_doc())
                .append(")"),
        }
    }
}
impl ToDoc for Literal {
    fn to_doc(&self) -> Doc {
        match self {
            Literal::Bool(_, b) => Doc::as_string(b),
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
            InfixOperator::Mul => Doc::text("*"),
            InfixOperator::Div => Doc::text("/"),
            InfixOperator::Eq => Doc::text("="),
            InfixOperator::Neq => Doc::text("≠"),
            InfixOperator::Lt => Doc::text("<"),
            InfixOperator::Lte => Doc::text("≤"),
            InfixOperator::Gt => Doc::text(">"),
            InfixOperator::Gte => Doc::text("≥"),
            InfixOperator::And => Doc::text("and"),
            InfixOperator::Or => Doc::text("or"),
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
