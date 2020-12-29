use std::path::PathBuf;

use codespan_reporting::files::SimpleFiles;
use cytosol_syntax::pretty_print;

use crate::reporting::{report_any_lexing_errors, report_parse_error};

pub(crate) fn dump_ast(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
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

        let ast = match cytosol_parser::parse_file(id, &toks) {
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

pub(crate) fn dump_tokens(paths: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
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
