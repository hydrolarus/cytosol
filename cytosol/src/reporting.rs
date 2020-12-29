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
            let label = Label::primary(tok.fc.file, tok.fc.range());
            let diag = Diagnostic::error()
                .with_message("Lexer error - Unknown token")
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
        ParseError::UnexpectedToken(fc) => {
            let label = Label::primary(fc.file, fc.range());
            let diag = Diagnostic::error()
                .with_message("Parse error - Unexpected token")
                .with_labels(vec![label]);
            let mut writer = StandardStream::stderr(ColorChoice::Auto);
            let mut config = Config::default();
            config.start_context_lines = 4;
            config.end_context_lines = 4;

            codespan_reporting::term::emit(&mut writer, &config, files, &diag).unwrap();
        }
        ParseError::UnexpectedEnd => {}
    }
}
