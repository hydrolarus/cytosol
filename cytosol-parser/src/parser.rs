use thiserror::Error;

use cytosol_syntax::{
    AtomBinding, Enzyme, Expression, Extern, File, Gene, GeneStatement, HasFC, Identifier,
    InfixOperator, Literal, PrefixOperator, Product, Quantified, Type, FC,
};

use crate::{lexer::TokenKind, Token};

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unexpected token at {:?}", .0)]
    UnexpectedToken(FC),
    #[error("Unexpected end")]
    UnexpectedEnd,
}

type Result<T> = core::result::Result<T, Error>;

pub fn parse_file<'src>(tokens: &'src [Token<'src>]) -> Result<File> {
    let mut p = Parser { toks: tokens };
    p.parse_file()
}

struct Parser<'src> {
    toks: &'src [Token<'src>],
}

impl<'src> Parser<'src> {
    fn parse_file(&mut self) -> Result<File> {
        let mut file = File::default();

        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::Extern => {
                    let start_tok = self.next().unwrap();

                    let name = self.parse_identifier()?;

                    let (fc, params) = self.grouped_separated(
                        (TokenKind::ParenOpen, TokenKind::ParenClose),
                        TokenKind::Comma,
                        |s| {
                            let ident = s.parse_identifier()?;
                            let _ = s.expect_tok_and_fc(|t| matches!(t.kind, TokenKind::Colon))?;
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
                        TokenKind::Comma,
                        |s| s.parse_quantified(Self::parse_atom_binding),
                    )?;

                    let (end_fc, stmts) = self.grouped(
                        (TokenKind::BraceOpen, TokenKind::BraceClose),
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

                    let name = self.parse_identifier()?;

                    self.expect(|t| t.kind == TokenKind::Colon)?;

                    let (_, reactants) = self.parse_atom_binding_list()?;

                    self.expect(|t| t.kind == TokenKind::ArrowR)?;

                    let (end_fc, products) = self.parse_product_list()?;

                    let fc = start_tok.fc.merge(end_fc);
                    file.enzymes.push(Enzyme {
                        fc,
                        name,
                        reactants,
                        products,
                    });
                }
                _ => return Err(Error::UnexpectedToken(t.fc.clone())),
            }
        }

        Ok(file)
    }

    fn parse_gene_statement(&mut self) -> Result<GeneStatement> {
        let next = self.peek().ok_or(Error::UnexpectedEnd)?;

        match next.kind {
            TokenKind::Call => {
                let call_tok = self.next().unwrap();
                let name = self.parse_identifier()?;
                let (end_fc, arguments) = self.grouped_separated(
                    (TokenKind::ParenOpen, TokenKind::ParenClose),
                    TokenKind::Comma,
                    |s| {
                        let name = s.parse_identifier()?;
                        let _ = s.expect_tok_and_fc(|t| matches!(t.kind, TokenKind::Colon))?;
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
                Ok(GeneStatement::Express(expr_tok.fc.clone(), prod))
            }
            _ => Err(Error::UnexpectedToken(next.fc.clone())),
        }
    }

    fn parse_atom_binding_list(&mut self) -> Result<(FC, Vec<Quantified<AtomBinding>>)> {
        let next = self.peek().ok_or(Error::UnexpectedEnd)?;

        if next.kind == TokenKind::Nothing {
            let _ = self.next();
            return Ok((next.fc.clone(), vec![]));
        }

        self.separated(TokenKind::OpPlus, |s| {
            s.parse_quantified(Self::parse_atom_binding)
        })
    }

    fn parse_product_list(&mut self) -> Result<(FC, Vec<Quantified<Product>>)> {
        let next = self.peek().ok_or(Error::UnexpectedEnd)?;

        if next.kind == TokenKind::Nothing {
            let _ = self.next();
            return Ok((next.fc.clone(), vec![]));
        }

        self.separated(TokenKind::OpPlus, |s| {
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
                    quantity: Some((t.fc.clone(), *n)),
                    value: f(self)?,
                })
            } else {
                Ok(Quantified {
                    quantity: None,
                    value: f(self)?,
                })
            }
        } else {
            Err(Error::UnexpectedEnd)
        }
    }

    fn parse_atom_binding(&mut self) -> Result<AtomBinding> {
        let name = self.parse_identifier()?;

        let (fc, fields) = if self.peek_kind(|t| t == &TokenKind::ParenOpen) {
            self.grouped_separated(
                (TokenKind::ParenOpen, TokenKind::ParenClose),
                TokenKind::Comma,
                |s| {
                    let name = s.parse_identifier()?;
                    let _ = s.expect(|t| t.kind == TokenKind::Colon)?;
                    let expr = s.parse_type()?;
                    Ok((name, expr))
                },
            )?
        } else {
            (name.fc(), vec![])
        };

        Ok(AtomBinding {
            fc: name.fc().merge(fc),
            name,
            fields,
        })
    }

    fn parse_type(&mut self) -> Result<Type> {
        let id = self.parse_identifier()?;
        match id.1.as_str() {
            "int" => Ok(Type::Int(id.fc())),
            "string" => Ok(Type::String(id.fc())),
            _ => todo!(),
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier> {
        let (fc, id) = self.expect_tok_and_fc(|t| {
            if let TokenKind::Identifier(i) = t.kind {
                Some(i)
            } else {
                None
            }
        })?;
        Ok(Identifier(fc.clone(), id.to_string()))
    }

    fn parse_product(&mut self) -> Result<Product> {
        let name = self.parse_identifier()?;

        let (fc, fields) = if self.peek_kind(|t| t == &TokenKind::ParenOpen) {
            self.grouped_separated(
                (TokenKind::ParenOpen, TokenKind::ParenClose),
                TokenKind::Comma,
                |s| {
                    let name = s.parse_identifier()?;
                    let _ = s.expect(|t| t.kind == TokenKind::Colon)?;
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
                TokenKind::OpPlus => (next.fc.clone(), InfixOperator::Add),
                TokenKind::OpMinus => (next.fc.clone(), InfixOperator::Sub),
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
        let next = self.peek().ok_or(Error::UnexpectedEnd)?;

        match &next.kind {
            TokenKind::Identifier(n) => {
                let _ = self.next();
                Ok(Expression::Variable(Identifier(
                    next.fc.clone(),
                    n.to_string(),
                )))
            }
            TokenKind::IntegerLiteral(i) => {
                let _ = self.next();
                Ok(Expression::Literal(Literal::Integer(next.fc.clone(), *i)))
            }
            TokenKind::StringLiteral(s) => {
                let _ = self.next();
                Ok(Expression::Literal(Literal::String(
                    next.fc.clone(),
                    s.clone(),
                )))
            }
            TokenKind::ParenOpen => {
                let _ = self.next();
                let val = self.parse_expression()?;
                self.expect(|t| t.kind == TokenKind::ParenClose)?;
                Ok(val)
            }
            TokenKind::OpMinus => {
                let t = self.next().unwrap();
                let rhs = self.parse_expression_atom()?;
                Ok(Expression::PrefixOp {
                    op: (t.fc.clone(), PrefixOperator::Neg),
                    expr: Box::new(rhs),
                })
            }
            _ => Err(Error::UnexpectedToken(next.fc.clone())),
        }
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

    fn expect<R: ExpectRet>(&mut self, f: impl FnOnce(&'src Token<'src>) -> R) -> Result<R::Out> {
        match self.toks {
            [tok, rest @ ..] => match f(tok).as_result(&tok.fc) {
                Ok(val) => {
                    self.toks = rest;
                    Ok(val)
                }
                Err(err) => Err(err),
            },
            [] => Err(Error::UnexpectedEnd),
        }
    }

    fn expect_tok_and_fc<R: ExpectRet>(
        &mut self,
        f: impl FnOnce(&'src Token<'src>) -> R,
    ) -> Result<(&'src FC, R::Out)> {
        self.expect(|tok| f(tok).as_result(&tok.fc).map(|r| (&tok.fc, r)))
    }

    fn grouped<T>(
        &mut self,
        delim: (TokenKind<'src>, TokenKind<'src>),
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(FC, Vec<T>)> {
        let mut vals = vec![];

        let (start_fc, ()) = self.expect_tok_and_fc(|t| t.kind == delim.0)?;

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
        separator: TokenKind<'src>,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(FC, Vec<T>)> {
        let mut vals = vec![];

        let (start_fc, ()) = self.expect_tok_and_fc(|t| t.kind == delim.0)?;

        loop {
            if self.peek().map(|t| &t.kind) == Some(&delim.1) {
                let end_fc = &self.next().unwrap().fc;
                let fc = start_fc.merge(end_fc);
                return Ok((fc, vals));
            }

            vals.push(f(self)?);

            let (fc, end) = self.expect_tok_and_fc(|tok| {
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
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(FC, Vec<T>)> {
        let mut vals = vec![];

        let start_fc = &self.peek().ok_or(Error::UnexpectedEnd)?.fc;

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

    fn as_result(self, fc: &FC) -> Result<Self::Out>;
}

impl<T> ExpectRet for Option<T> {
    type Out = T;

    fn as_result(self, fc: &FC) -> Result<Self::Out> {
        match self {
            Some(val) => Ok(val),
            None => Err(Error::UnexpectedToken(fc.clone())),
        }
    }
}

impl<T> ExpectRet for Result<T> {
    type Out = T;

    fn as_result(self, _: &FC) -> Result<Self::Out> {
        self
    }
}

impl ExpectRet for bool {
    type Out = ();

    fn as_result(self, fc: &FC) -> Result<Self::Out> {
        match self {
            true => Ok(()),
            false => Err(Error::UnexpectedToken(fc.clone())),
        }
    }
}
