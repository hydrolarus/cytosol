use cytosol_syntax::{FileId, FC};
use logos::Logos;

pub struct Token<'src> {
    pub fc: FC,
    pub kind: TokenKind<'src>,
}

#[derive(Debug, Logos, PartialEq, Eq)]
pub enum TokenKind<'src> {
    #[token("record")]
    Record,

    #[token("gene")]
    Gene,

    #[token("rule")]
    Rule,

    #[token("express")]
    Express,

    #[token("call")]
    Call,

    #[token("extern")]
    Extern,

    #[token("when")]
    When,

    #[token("Ø")]
    #[token("ø")]
    #[token("nothing")]
    Nothing,

    #[regex(r"(\p{XID_Start}|_)(\p{XID_Continue}|')*")]
    Identifier(&'src str),

    #[regex(r"[0-9][_0-9]*", |lex| parse_integer_literal(lex.slice()))]
    IntegerLiteral(usize),

    #[token("\"", parse_string_literal)]
    StringLiteral(String),

    #[token("->")]
    #[token("→")]
    ArrowR,

    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,

    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    #[token("-")]
    OpMinus,
    #[token("+")]
    OpPlus,

    #[token("*")]
    OpStar,
    #[token("/")]
    OpSlash,

    #[token("=")]
    OpEquals,
    #[token("/=")]
    #[token("!=")]
    #[token("≠")]
    OpNotEquals,
    #[token("<")]
    OpLessThan,
    #[token("<=")]
    #[token("≤")]
    OpLessThanEqual,
    #[token(">")]
    OpGreaterThan,
    #[token(">=")]
    #[token("≥")]
    OpGreaterThanEqual,

    #[token("and")]
    OpAnd,
    #[token("or")]
    OpOr,

    #[error]
    // skip whitespace
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    // skip comments
    #[regex(r"(//[^\n]*)|(//[^(\r\n)]*)", logos::skip)]
    Error,
}

pub fn tokenise(file: FileId, input: &str) -> impl Iterator<Item = Token<'_>> {
    TokenKind::lexer(input).spanned().map(move |(tok, span)| {
        let fc = FC {
            file,
            start: span.start,
            end: span.end,
        };
        Token { kind: tok, fc }
    })
}

fn parse_integer_literal(s: &str) -> Option<usize> {
    let mut acc: usize = 0;

    for c in s.chars() {
        if c == '_' {
            continue;
        }

        let digit = c as u32 - '0' as u32;

        acc = acc.checked_mul(10)?;
        acc = acc.checked_add(digit as usize)?;
    }

    Some(acc)
}

fn parse_string_literal<'src>(lex: &mut logos::Lexer<'src, TokenKind<'src>>) -> Option<String> {
    let s = lex.remainder();

    let mut buf = String::new();

    let mut escape = false;

    for c in s.chars() {
        if escape {
            match c {
                'n' => buf.push('\n'),
                'r' => buf.push('\r'),
                '\\' => buf.push('\\'),
                _ => return None,
            }
            lex.bump(1);
            escape = false;
        } else {
            if c == '"' {
                lex.bump(1);
                return Some(buf);
            }

            if c == '\\' {
                escape = true;
                lex.bump(1);
                continue;
            }

            buf.push(c);
            lex.bump(c.len_utf8());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    use codespan_reporting::files::SimpleFiles;

    #[test]
    fn string_literal() {
        let input = r#"
            "hello world!\n"
            ""
            "\r\n\nHello"
        "#;

        let mut files = SimpleFiles::new();
        let id = files.add("<test>", input);

        let toks = tokenise(id, input).collect::<Vec<_>>();
        assert_eq!(toks.len(), 3);
        assert_eq!(
            toks[0].kind,
            TokenKind::StringLiteral("hello world!\n".to_string())
        );
        assert_eq!(toks[1].kind, TokenKind::StringLiteral("".to_string()));
        assert_eq!(
            toks[2].kind,
            TokenKind::StringLiteral("\r\n\nHello".to_string())
        );
    }

    #[test]
    fn integer_literal() {
        let input = r#"
        12
        0
        493
        10_000_000
        "#;

        let mut files = SimpleFiles::new();
        let id = files.add("<test>", input);

        let toks = tokenise(id, input).collect::<Vec<_>>();
        assert_eq!(toks.len(), 4);
        assert_eq!(toks[0].kind, TokenKind::IntegerLiteral(12));
        assert_eq!(toks[1].kind, TokenKind::IntegerLiteral(0));
        assert_eq!(toks[2].kind, TokenKind::IntegerLiteral(493));
        assert_eq!(toks[3].kind, TokenKind::IntegerLiteral(10_000_000));
    }

    #[test]
    fn identifiers() {
        let input = "A53α";

        let mut files = SimpleFiles::new();
        let id = files.add("<test>", input);

        let toks = tokenise(id, input).collect::<Vec<_>>();
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].kind, TokenKind::Identifier("A53α"));
    }
}
