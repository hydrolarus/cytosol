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

    file_paths: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    if args.dump_tokens {
        debug::dump_tokens(&args.file_paths)?;
    } else if args.dump_ast {
        debug::dump_ast(&args.file_paths)?;
    } else {
        run_files(&args.file_paths)?;
    }

    Ok(())
}

fn run_files(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    for path in paths {
        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.file_name().and_then(|s| s.to_str()).unwrap(), content);

        let toks =
            cytosol_parser::tokenise(id, files.get(id).unwrap().source()).collect::<Vec<_>>();

        let found_errors = reporting::report_any_lexing_errors(&files, &toks);

        if found_errors {
            continue;
        }

        let ast = match cytosol_parser::parse_file(id, &toks) {
            Ok(val) => val,
            Err(err) => {
                reporting::report_parse_error(&files, err);
                continue;
            }
        };

        // TODO run the file or something
        let _ = ast;
    }

    Ok(())
}
