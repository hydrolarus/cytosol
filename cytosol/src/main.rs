use std::path::PathBuf;

use clap::Clap;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use cytosol_parser::{ParseError, Token, TokenKind};
use cytosol_syntax::pretty_print;

#[derive(Debug, Clap)]
#[clap(version = "0.1", author = "Tia")]
struct Arguments {
    #[clap(long)]
    dump_tokens: bool,

    #[clap(long)]
    dump_ast: bool,

    file_paths: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    if args.dump_tokens {
        dump_tokens(&args.file_paths)?;
    } else if args.dump_ast {
        dump_ast(&args.file_paths)?;
    } else {
        // TODO
        println!("Not implemented yet, dumping AST");
        dump_ast(&args.file_paths)?;
    }

    Ok(())
}

fn dump_ast(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    for path in paths {
        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.file_name().and_then(|s| s.to_str()).unwrap(), content);

        let toks =
            cytosol_parser::tokenise(id, files.get(id).unwrap().source()).collect::<Vec<_>>();

        let found_errors = report_any_lexing_errors(&files, &toks);

        if found_errors {
            continue;
        }

        let ast = match cytosol_parser::parse_file(&toks) {
            Ok(val) => val,
            Err(err) => {
                report_parse_error(&files, err);
                continue;
            }
        };

        println!("{}", pretty_print(&ast, 80));
    }

    Ok(())
}

fn dump_tokens(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    for path in paths {
        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.file_name().and_then(|s| s.to_str()).unwrap(), content);

        let toks = cytosol_parser::tokenise(id, files.get(id).unwrap().source());

        for tok in toks {
            println!("{:?}", tok.kind);
        }
    }

    Ok(())
}

fn report_any_lexing_errors(files: &SimpleFiles<&str, String>, toks: &[Token<'_>]) -> bool {
    let mut found = false;
    for tok in toks {
        if tok.kind == TokenKind::Error {
            let label = Label::new(LabelStyle::Primary, tok.fc.file, tok.fc.range());
            let diag = Diagnostic::error()
                .with_message("Unknown token")
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

fn report_parse_error(files: &SimpleFiles<&str, String>, err: ParseError) {
    match err {
        ParseError::UnexpectedToken(fc) => {
            let label = Label::new(LabelStyle::Primary, fc.file, fc.range());
            let diag = Diagnostic::error()
                .with_message("Unexpected token")
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
