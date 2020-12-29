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
                .with_message(format!("Unknown token `{}`", source))
                .with_labels(vec![label]);

            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            let mut config = Config::default();
            config.start_context_lines = 4;
            config.end_context_lines = 4;

            codespan_reporting::term::emit(&mut writer, &config, files, &diag).unwrap();

            found = true;
        }
    }
    found
}

pub(crate) fn report_parse_error<'a>(files: &'a impl Files<'a, FileId = usize>, err: ParseError) {
    match err {
        ParseError::UnexpectedToken(fc, desc) => {
            let file_source = files.source(fc.file).expect("Invalid file ID");
            let source = &file_source.as_ref()[fc.start..fc.end];

            let message = if let Some(exp) = desc.expected {
                format!("expected {} but found `{}`", exp, source)
            } else {
                format!("unexpected token `{}`", source)
            };

            let label = Label::primary(fc.file, fc.range()).with_message("Unexpected token");
            let diag = Diagnostic::error()
                .with_code("parse-error")
                .with_message(message)
                .with_labels(vec![label])
                .with_notes(vec![format!(
                    "unexpected token while parsing {}",
                    desc.item_context
                )]);
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            let mut config = Config::default();
            config.start_context_lines = 4;
            config.end_context_lines = 4;

            codespan_reporting::term::emit(&mut writer, &config, files, &diag).unwrap();
        }
        ParseError::UnexpectedEnd(file, desc) => {
            let message = if let Some(exp) = desc.expected {
                format!("expected {} but reached end of file", exp)
            } else {
                "unexpected end of file".to_string()
            };

            let len = files.source(file).expect("Invalid file ID").as_ref().len();

            let label = Label::primary(file, len..len);
            let diag = Diagnostic::error()
                .with_code("parse-error")
                .with_message(message)
                .with_labels(vec![label])
                .with_notes(vec![format!(
                    "unexpected end of file while parsing {}",
                    desc.item_context
                )]);
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            let mut config = Config::default();
            config.start_context_lines = 4;
            config.end_context_lines = 4;

            codespan_reporting::term::emit(&mut writer, &config, files, &diag).unwrap();
        }
    }
}
