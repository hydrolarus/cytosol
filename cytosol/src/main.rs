use std::path::PathBuf;

use clap::Clap;
use codespan_reporting::files::SimpleFiles;

mod debug;
mod reporting;

#[derive(Debug, Clap)]
#[clap(version = "0.1", author = "Tia")]
struct Arguments {
    #[clap(long)]
    dump_tokens: bool,

    #[clap(long)]
    dump_ast: bool,

    /// Do not display colours in the terminal output
    #[clap(long)]
    no_colour: bool,

    file_paths: Vec<PathBuf>,
}

pub(crate) struct Config {
    // Do not display colours in the terminal
    pub(crate) no_colour: bool,
}

impl Config {
    fn from_arguments(args: &Arguments) -> Self {
        Self {
            no_colour: args.no_colour,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();
    let config = Config::from_arguments(&args);

    if args.dump_tokens {
        debug::dump_tokens(&config, &args.file_paths)?;
    } else if args.dump_ast {
        debug::dump_ast(&config, &args.file_paths)?;
    } else {
        run_files(&config, &args.file_paths)?;
    }

    Ok(())
}

fn run_files(config: &Config, paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    let mut parsed_files = vec![];

    for path in paths {
        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.to_str().unwrap(), content);

        let toks =
            cytosol_parser::tokenise(id, files.get(id).unwrap().source()).collect::<Vec<_>>();

        let found_errors = reporting::report_any_lexing_errors(config, &files, &toks);

        if found_errors {
            continue;
        }

        let ast = match cytosol_parser::parse_file(id, &toks) {
            Ok(val) => val,
            Err(err) => {
                reporting::report_parse_error(config, &files, err);
                continue;
            }
        };

        parsed_files.push(ast);
    }

    // TODO run the file or something
    let mut prog = cytosol_hir::Program::new();

    if let Some(errs) = cytosol_hir::ast_to_hir::files_to_hir(&mut prog, &parsed_files) {
        reporting::report_hir_translate_errors(config, &files, errs);
    }

    Ok(())
}
