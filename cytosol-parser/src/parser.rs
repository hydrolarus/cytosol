use thiserror::Error;

use cytosol_syntax::{AtomBinding, Extern, File, Gene, HasFC, Identifier, Quantified, Type, FC};

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
                    let (start_fc, ()) =
                        self.expect_tok_and_fc(|t| matches!(t.kind, TokenKind::Extern))?;

                    let name = self.parse_identifier()?;

                    let (fc, params) = self.grouped_separated(
                        (TokenKind::ParenOpen, TokenKind::ParenClose),
                        Some(TokenKind::Comma),
                        |s| {
                            let ident = s.parse_identifier()?;
                            let _ = s.expect_tok_and_fc(|t| matches!(t.kind, TokenKind::Colon))?;
                            let ty = s.parse_type()?;
                            Ok((ident, ty))
                        },
                    )?;

                    let fc = start_fc.merge(fc);
                    file.externs.push(Extern {
                        fc,
                        name,
                        parameters: params,
                    });
                }
                TokenKind::Gene => {
                    let (start_fc, ()) =
                        self.expect_tok_and_fc(|t| matches!(t.kind, TokenKind::Gene))?;

                    let (_, factors) = self.grouped_separated(
                        (TokenKind::BracketOpen, TokenKind::BracketClose),
                        Some(TokenKind::Comma),
                        |s| s.parse_quantified(Self::parse_atom_binding),
                    )?;

                    self.expect(|t| matches!(t.kind, TokenKind::BraceOpen))?;

                    let (end_fc, ()) =
                        self.expect_tok_and_fc(|t| matches!(t.kind, TokenKind::BraceClose))?;

                    let fc = start_fc.merge(end_fc);

                    file.genes.push(Gene {
                        fc,
                        factors,
                        body: vec![],
                    });
                }
                TokenKind::Enzyme => {}
                _ => return Err(Error::UnexpectedToken(t.fc.clone())),
            }
        }

        Ok(file)
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
        let id = self.parse_identifier()?;
        // TODO fields
        Ok(AtomBinding {
            fc: id.fc(),
            name: id,
            fields: vec![],
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
}

/// Utilities
impl<'src> Parser<'src> {
    fn peek(&self) -> Option<&'src Token<'src>> {
        self.toks.first()
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

    fn grouped_separated<T>(
        &mut self,
        delim: (TokenKind<'src>, TokenKind<'src>),
        separator: Option<TokenKind<'src>>,
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

            match self.peek() {
                Some(tok) => {
                    if tok.kind == delim.1 {
                        let end_fc = &self.next().unwrap().fc;
                        let fc = start_fc.merge(end_fc);
                        return Ok((fc, vals));
                    }

                    if let Some(sep) = &separator {
                        if &tok.kind == sep {
                            let _ = self.next();
                        } else {
                            return Err(Error::UnexpectedToken(tok.fc.clone()));
                        }
                    }
                }
                None => {
                    return Err(Error::UnexpectedEnd);
                }
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
