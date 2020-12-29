use thiserror::Error;

use cytosol_syntax::{
    Atom, AtomBinding, AtomBindingAttribute, Enzyme, Expression, Extern, File, FileId, Gene,
    GeneStatement, HasFC, Identifier, InfixOperator, Literal, PrefixOperator, Product, Quantified,
    Type, FC,
};

use crate::{lexer::TokenKind, Token};

#[derive(Debug, Clone, Copy)]
pub struct ErrorContext {
    pub while_parsing: &'static str,
    pub expected: Option<&'static str>,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unexpected token at {:?}", .0)]
    UnexpectedToken(FC, ErrorContext),
    #[error("Unexpected end")]
    UnexpectedEnd(FileId, ErrorContext),
}

fn ctx_(while_parsing: &'static str) -> ErrorContext {
    ErrorContext {
        while_parsing,
        expected: None,
    }
}

fn ctx(while_parsing: &'static str, expected: &'static str) -> ErrorContext {
    ErrorContext {
        while_parsing,
        expected: Some(expected),
    }
}

type Result<T> = core::result::Result<T, Error>;

pub fn parse_file<'src>(file: FileId, tokens: &'src [Token<'src>]) -> Result<File> {
    let mut p = Parser { file, toks: tokens };
    p.parse_file()
}

struct Parser<'src> {
    file: FileId,
    toks: &'src [Token<'src>],
}

impl<'src> Parser<'src> {
    fn parse_file(&mut self) -> Result<File> {
        let mut file = File::default();

        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::Atom => {
                    let start_tok = self.next().unwrap();

                    let name = self.parse_identifier("an atom definition")?;

                    let (fc, fields) = if self.peek_kind(|t| t == &TokenKind::ParenOpen) {
                        self.grouped_separated(
                            (TokenKind::ParenOpen, TokenKind::ParenClose),
                            ctx("the field list of an atom item", "`(`"),
                            TokenKind::Comma,
                            ctx("the field list of an atom item", "`,` or `)`"),
                            |s| {
                                let ident = s.parse_identifier("an atom field")?;
                                let _ = s.expect_tok_and_fc(ctx("an atom field", "`:`"), |t| {
                                    matches!(t.kind, TokenKind::Colon)
                                })?;
                                let ty = s.parse_type()?;
                                Ok((ident, ty))
                            },
                        )?
                    } else {
                        (name.fc(), vec![])
                    };

                    file.atoms.push(Atom {
                        fc: start_tok.fc.merge(fc),
                        name,
                        fields,
                    });
                }
                TokenKind::Extern => {
                    let start_tok = self.next().unwrap();

                    let name = self.parse_identifier("an extern item")?;

                    let (fc, params) = self.grouped_separated(
                        (TokenKind::ParenOpen, TokenKind::ParenClose),
                        ctx("the parameter list of an extern item", "`(`"),
                        TokenKind::Comma,
                        ctx("the parameter list of an extern item", "`,` or `)`"),
                        |s| {
                            let ident = s.parse_identifier("an extern item parameter")?;
                            let _ = s.expect_tok_and_fc(
                                ctx("an extern parameter description", "`:`"),
                                |t| matches!(t.kind, TokenKind::Colon),
                            )?;
                            let ty = s.parse_type()?;
                            Ok((ident, ty))
                        },
                    )?;

                    let fc = start_tok.fc.merge(fc);
                    file.externs.push(Extern {
                        fc,
                        name,
                        parameters: params,
                    });
                }
                TokenKind::Gene => {
                    let start_tok = self.next().unwrap();

                    let (_, factors) = self.grouped_separated(
                        (TokenKind::BracketOpen, TokenKind::BracketClose),
                        ctx("a gene factor list", "`[`"),
                        TokenKind::Comma,
                        ctx("a gene factor list", "`,` or `]`"),
                        Self::parse_atom_binding,
                    )?;

                    let (end_fc, stmts) = self.grouped(
                        (TokenKind::BraceOpen, TokenKind::BraceClose),
                        ctx("a gene statement list", "`{`"),
                        Self::parse_gene_statement,
                    )?;

                    let fc = start_tok.fc.merge(end_fc);

                    file.genes.push(Gene {
                        fc,
                        factors,
                        body: stmts,
                    });
                }
                TokenKind::Enzyme => {
                    let start_tok = self.next().unwrap();

                    let name = self.parse_identifier("an enzyme item")?;

                    let (_, reactants) = self.grouped_separated(
                        (TokenKind::BracketOpen, TokenKind::BracketClose),
                        ctx("an enzyme reactant list", "`[`"),
                        TokenKind::Comma,
                        ctx("an enzyme reactant list", "`,` or `]`"),
                        Self::parse_atom_binding,
                    )?;

                    self.expect(ctx("an enzyme reaction description", "`->`"), |t| {
                        t.kind == TokenKind::ArrowR
                    })?;

                    let (end_fc, products) = self.parse_product_list()?;

                    let fc = start_tok.fc.merge(end_fc);
                    file.enzymes.push(Enzyme {
                        fc,
                        name,
                        reactants,
                        products,
                    });
                }
                _ => return Err(Error::UnexpectedToken(t.fc, ctx_("a top level item"))),
            }
        }

        Ok(file)
    }

    fn parse_gene_statement(&mut self) -> Result<GeneStatement> {
        let next = self
            .peek()
            .ok_or_else(|| Error::UnexpectedEnd(self.file, ctx_("a gene statement")))?;

        match next.kind {
            TokenKind::Call => {
                let call_tok = self.next().unwrap();
                let name = self.parse_identifier("a call statement")?;
                let (end_fc, arguments) = self.grouped_separated(
                    (TokenKind::ParenOpen, TokenKind::ParenClose),
                    ctx("a call statement parameter list", "`(`"),
                    TokenKind::Comma,
                    ctx("a call statement parameter list", "`,` or `)`"),
                    |s| {
                        let name = s.parse_identifier("a named argument")?;
                        let _ = s.expect_tok_and_fc(ctx("a named argument", "`:`"), |t| {
                            matches!(t.kind, TokenKind::Colon)
                        })?;
                        let val = s.parse_expression()?;
                        Ok((name, val))
                    },
                )?;
                let fc = call_tok.fc.merge(end_fc);
                Ok(GeneStatement::Call {
                    fc,
                    name,
                    arguments,
                })
            }
            TokenKind::Express => {
                let expr_tok = self.next().unwrap();
                let prod = self.parse_quantified(Self::parse_product)?;
                Ok(GeneStatement::Express(expr_tok.fc, prod))
            }
            _ => Err(Error::UnexpectedToken(next.fc, ctx_("a gene statement"))),
        }
    }

    fn parse_product_list(&mut self) -> Result<(FC, Vec<Quantified<Product>>)> {
        let next = self
            .peek()
            .ok_or_else(|| Error::UnexpectedEnd(self.file, ctx_("a product list")))?;

        if next.kind == TokenKind::Nothing {
            let _ = self.next();
            return Ok((next.fc, vec![]));
        }

        self.separated(TokenKind::OpPlus, ctx_("a product list"), |s| {
            s.parse_quantified(Self::parse_product)
        })
    }

    fn parse_quantified<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<Quantified<T>> {
        if let Some(t) = self.peek() {
            if let TokenKind::IntegerLiteral(n) = &t.kind {
                let _ = self.next();
                Ok(Quantified {
                    quantity: Some((t.fc, *n)),
                    value: f(self)?,
                })
            } else {
                Ok(Quantified {
                    quantity: None,
                    value: f(self)?,
                })
            }
        } else {
            Err(Error::UnexpectedEnd(self.file, ctx_("a quantified item")))
        }
    }

    fn parse_atom_binding(&mut self) -> Result<AtomBinding> {
        let next = self.peek().ok_or_else(|| {
            Error::UnexpectedEnd(
                self.file,
                ctx("an atom binding", "a quantity or identifier"),
            )
        })?;

        match &next.kind {
            TokenKind::IntegerLiteral(n) => {
                let _ = self.next();
                let attr = AtomBindingAttribute::Quantity(next.fc, *n);

                let name = self.parse_identifier("an atom binding")?;

                Ok(AtomBinding {
                    fc: next.fc.merge(name.fc()),
                    name,
                    attr: Some(attr),
                })
            }
            TokenKind::Identifier(_) => {
                let id = self.parse_identifier("an atom binding")?;

                if let Some(next) = self.peek() {
                    if next.kind == TokenKind::Colon {
                        let _ = self.next();

                        let name = self.parse_identifier("an atom binding")?;

                        Ok(AtomBinding {
                            fc: id.fc().merge(name.fc()),
                            name,
                            attr: Some(AtomBindingAttribute::Name(id)),
                        })
                    } else {
                        Ok(AtomBinding {
                            fc: id.fc(),
                            name: id,
                            attr: None,
                        })
                    }
                } else {
                    Ok(AtomBinding {
                        fc: id.fc(),
                        name: id,
                        attr: None,
                    })
                }
            }
            _ => Err(Error::UnexpectedToken(
                next.fc,
                ctx("an atom binding", "a quantity or identifier"),
            )),
        }
    }

    fn parse_type(&mut self) -> Result<Type> {
        let id = self.parse_identifier("a type")?;
        Ok(Type::Named(id))
    }

    fn parse_identifier(&mut self, while_parsing: &'static str) -> Result<Identifier> {
        let ctx = ctx(while_parsing, "an identifier");

        let (fc, id) = self.expect_tok_and_fc(ctx, |t| {
            if let TokenKind::Identifier(i) = t.kind {
                Some(i)
            } else {
                None
            }
        })?;
        Ok(Identifier(fc, id.to_string()))
    }

    fn parse_product(&mut self) -> Result<Product> {
        let name = self.parse_identifier("a product")?;

        let (fc, fields) = if self.peek_kind(|t| t == &TokenKind::ParenOpen) {
            self.grouped_separated(
                (TokenKind::ParenOpen, TokenKind::ParenClose),
                ctx("the start of product fields", "`(`"),
                TokenKind::Comma,
                ctx("a product field list", "`,` or `)`"),
                |s| {
                    let name = s.parse_identifier("a product field")?;
                    let _ = s.expect(ctx("a product field", "`:`"), |t| {
                        t.kind == TokenKind::Colon
                    })?;
                    let expr = s.parse_expression()?;
                    Ok((name, expr))
                },
            )?
        } else {
            (name.fc(), vec![])
        };

        Ok(Product {
            fc: name.fc().merge(fc),
            name,
            fields,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        let mut expr = self.parse_expression_atom()?;

        while let Some(next) = self.peek() {
            let op = match next.kind {
                TokenKind::OpPlus => (next.fc, InfixOperator::Add),
                TokenKind::OpMinus => (next.fc, InfixOperator::Sub),
                _ => return Ok(expr),
            };

            let _ = self.next();

            let rhs = self.parse_expression_atom()?;

            expr = Expression::InfixOp {
                op,
                args: Box::new([expr, rhs]),
            };
        }

        Ok(expr)
    }

    fn parse_expression_atom(&mut self) -> Result<Expression> {
        let next = self
            .peek()
            .ok_or_else(|| Error::UnexpectedEnd(self.file, ctx_("an expression atom")))?;

        let mut expr = match &next.kind {
            TokenKind::Identifier(n) => {
                let _ = self.next();
                Expression::Variable(Identifier(next.fc, n.to_string()))
            }
            TokenKind::IntegerLiteral(i) => {
                let _ = self.next();
                Expression::Literal(Literal::Integer(next.fc, *i))
            }
            TokenKind::StringLiteral(s) => {
                let _ = self.next();
                Expression::Literal(Literal::String(next.fc, s.clone()))
            }
            TokenKind::ParenOpen => {
                let _ = self.next();
                let val = self.parse_expression()?;
                self.expect(ctx("a nested expression", "`)`"), |t| {
                    t.kind == TokenKind::ParenClose
                })?;
                val
            }
            TokenKind::OpMinus => {
                let t = self.next().unwrap();
                let rhs = self.parse_expression_atom()?;
                Expression::PrefixOp {
                    op: (t.fc, PrefixOperator::Neg),
                    expr: Box::new(rhs),
                }
            }
            _ => return Err(Error::UnexpectedToken(next.fc, ctx_("an expression atom"))),
        };

        while let Some(next) = self.peek() {
            if next.kind == TokenKind::Dot {
                let _ = self.next();

                let name = self.parse_identifier("a filed access expression")?;

                expr = Expression::FieldAccess {
                    base: Box::new(expr),
                    field_name: name,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }
}

/// Utilities
impl<'src> Parser<'src> {
    fn peek(&self) -> Option<&'src Token<'src>> {
        self.toks.first()
    }

    fn peek_kind(&self, f: impl FnOnce(&'src TokenKind<'src>) -> bool) -> bool {
        if let Some(tok) = self.peek() {
            f(&tok.kind)
        } else {
            false
        }
    }

    fn next(&mut self) -> Option<&'src Token<'src>> {
        match self.toks {
            [tok, rest @ ..] => {
                self.toks = rest;
                Some(tok)
            }
            [] => None,
        }
    }

    fn expect<R: ExpectRet>(
        &mut self,
        context: ErrorContext,
        f: impl FnOnce(&'src Token<'src>) -> R,
    ) -> Result<R::Out> {
        match self.toks {
            [tok, rest @ ..] => match f(tok).as_result(context, tok.fc) {
                Ok(val) => {
                    self.toks = rest;
                    Ok(val)
                }
                Err(err) => Err(err),
            },
            [] => Err(Error::UnexpectedEnd(self.file, context)),
        }
    }

    fn expect_tok_and_fc<R: ExpectRet>(
        &mut self,
        context: ErrorContext,
        f: impl FnOnce(&'src Token<'src>) -> R,
    ) -> Result<(FC, R::Out)> {
        self.expect(context, |tok| {
            f(tok).as_result(context, tok.fc).map(|r| (tok.fc, r))
        })
    }

    fn grouped<T>(
        &mut self,
        delim: (TokenKind<'src>, TokenKind<'src>),
        start_delim_context: ErrorContext,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(FC, Vec<T>)> {
        let mut vals = vec![];

        let (start_fc, ()) = self.expect_tok_and_fc(start_delim_context, |t| t.kind == delim.0)?;

        loop {
            if self.peek().map(|t| &t.kind) == Some(&delim.1) {
                let end_fc = &self.next().unwrap().fc;
                let fc = start_fc.merge(end_fc);
                return Ok((fc, vals));
            }

            vals.push(f(self)?);
        }
    }

    fn grouped_separated<T>(
        &mut self,
        delim: (TokenKind<'src>, TokenKind<'src>),
        delim_start_context: ErrorContext,
        separator: TokenKind<'src>,
        separator_or_delim_end_context: ErrorContext,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(FC, Vec<T>)> {
        let mut vals = vec![];

        let (start_fc, ()) = self.expect_tok_and_fc(delim_start_context, |t| t.kind == delim.0)?;

        loop {
            if self.peek().map(|t| &t.kind) == Some(&delim.1) {
                let end_fc = &self.next().unwrap().fc;
                let fc = start_fc.merge(end_fc);
                return Ok((fc, vals));
            }

            vals.push(f(self)?);

            let (fc, end) = self.expect_tok_and_fc(separator_or_delim_end_context, |tok| {
                if tok.kind == delim.1 {
                    Some(true)
                } else if tok.kind == separator {
                    Some(false)
                } else {
                    None
                }
            })?;

            if end {
                let fc = start_fc.merge(fc);
                return Ok((fc, vals));
            }
        }
    }

    fn separated<T: HasFC>(
        &mut self,
        sep: TokenKind<'src>,
        context: ErrorContext,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(FC, Vec<T>)> {
        let mut vals = vec![];

        let start_fc = &self
            .peek()
            .ok_or(Error::UnexpectedEnd(self.file, context))?
            .fc;

        vals.push(f(self)?);

        loop {
            if let Some(t) = self.peek() {
                if t.kind != sep {
                    let last_fc = vals.last().unwrap().fc();
                    let fc = start_fc.merge(last_fc);
                    return Ok((fc, vals));
                } else {
                    let _ = self.next();

                    vals.push(f(self)?);
                }
            } else {
                let last_fc = vals.last().unwrap().fc();
                let fc = start_fc.merge(last_fc);
                return Ok((fc, vals));
            }
        }
    }
}

trait ExpectRet {
    type Out;

    fn as_result(self, context: ErrorContext, fc: FC) -> Result<Self::Out>;
}

impl<T> ExpectRet for Option<T> {
    type Out = T;

    fn as_result(self, context: ErrorContext, fc: FC) -> Result<Self::Out> {
        match self {
            Some(val) => Ok(val),
            None => Err(Error::UnexpectedToken(fc, context)),
        }
    }
}

impl<T> ExpectRet for Result<T> {
    type Out = T;

    fn as_result(self, _: ErrorContext, _: FC) -> Result<Self::Out> {
        self
    }
}

impl ExpectRet for bool {
    type Out = ();

    fn as_result(self, context: ErrorContext, fc: FC) -> Result<Self::Out> {
        match self {
            true => Ok(()),
            false => Err(Error::UnexpectedToken(fc, context)),
        }
    }
}
