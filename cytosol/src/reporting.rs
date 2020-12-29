use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::Files,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use cytosol_parser::{ParseError, Token, TokenKind};

pub(crate) fn report_any_lexing_errors<'a>(
    files: &'a impl Files<'a, FileId = usize>,
    toks: &[Token<'a>],
) -> bool {
    let mut found = false;
    for tok in toks {
        if tok.kind == TokenKind::Error {
            let file_source = files.source(tok.fc.file).expect("Invalid file ID");
            let source = &file_source.as_ref()[tok.fc.start..tok.fc.end];

            let label = Label::primary(tok.fc.file, tok.fc.range());
            let diag = Diagnostic::error()
                .with_code("lexer-error")
                .with_message(format!("Invalid token `{}`", source))
                .with_labels(vec![label]);

            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            let config = Config::default();

            codespan_reporting::term::emit(&mut writer, &config, files, &diag).unwrap();

            found = true;
        }
    }
    found
}

pub(crate) fn report_parse_error<'a>(files: &'a impl Files<'a, FileId = usize>, err: ParseError) {
    let diag = match err {
        ParseError::UnexpectedToken(fc, desc) => {
            let file_source = files.source(fc.file).expect("Invalid file ID");
            let source = &file_source.as_ref()[fc.start..fc.end];

            let (diag_message, label_message) = if let Some(exp) = desc.expected {
                (
                    format!("expected {}, found `{}`", exp, source),
                    format!("expected {}", exp),
                )
            } else {
                (
                    format!("unexpected token `{}`", source),
                    "unexpected token".to_string(),
                )
            };

            let mut labels = vec![Label::primary(fc.file, fc.range()).with_message(label_message)];

            if let Some((fc, ref_desc)) = desc.start {
                labels.push(
                    Label::secondary(fc.file, fc.range())
                        .with_message(format!("{} started here", ref_desc)),
                );
            }

            Diagnostic::error()
                .with_code("parse-error")
                .with_message(diag_message)
                .with_labels(labels)
                .with_notes(vec![format!("error while parsing {}", desc.while_parsing)])
        }
        ParseError::UnexpectedEnd(file, desc) => {
            let (diag_message, label_message) = if let Some(exp) = desc.expected {
                (
                    format!("expected {}, found end of file", exp),
                    format!("expected {}", exp),
                )
            } else {
                (
                    "unexpected end of file".to_string(),
                    "unexpected end of file".to_string(),
                )
            };

            let len = files.source(file).expect("Invalid file ID").as_ref().len();

            let mut labels = vec![Label::primary(file, len..len).with_message(label_message)];

            if let Some((fc, ref_desc)) = desc.start {
                labels.push(
                    Label::secondary(fc.file, fc.range())
                        .with_message(format!("{} started here", ref_desc)),
                );
            }

            Diagnostic::error()
                .with_code("parse-error")
                .with_message(diag_message)
                .with_labels(labels)
                .with_notes(vec![format!("error while parsing {}", desc.while_parsing)])
        }
    };

    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();

    codespan_reporting::term::emit(&mut writer, &config, files, &diag).unwrap();
}
