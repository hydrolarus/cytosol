use std::path::PathBuf;

use codespan_reporting::files::SimpleFiles;
use cytosol_syntax::pretty_print;

use crate::{
    reporting::{report_any_lexing_errors, report_parse_error},
    timing::{FileStage, FileSummary, TimingReport},
    Config,
};

pub(crate) fn dump_ast(
    timing: &mut TimingReport,
    config: &Config,
    paths: &[PathBuf],
) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    for path in paths {
        let mut file_summ = FileSummary::new(path);

        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.to_str().unwrap(), content);

        let toks = file_summ.record(FileStage::Lexing, || {
            cytosol_parser::tokenise(id, files.get(id).unwrap().source()).collect::<Vec<_>>()
        });

        let found_errors = report_any_lexing_errors(config, &files, &toks);

        if found_errors {
            continue;
        }

        let parse_res =
            file_summ.record(FileStage::Parsing, || cytosol_parser::parse_file(id, &toks));

        let ast = match parse_res {
            Ok(val) => val,
            Err(err) => {
                report_parse_error(config, &files, err);
                continue;
            }
        };

        eprintln!("{}", pretty_print(&ast, 80));

        timing.add_file(file_summ);
    }

    Ok(())
}

pub(crate) fn dump_tokens(
    timing: &mut TimingReport,
    _config: &Config,
    paths: &[PathBuf],
) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    for path in paths {
        let mut file_summ = FileSummary::new(path);

        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.to_str().unwrap(), content);

        let toks = file_summ.record(FileStage::Lexing, || {
            cytosol_parser::tokenise(id, files.get(id).unwrap().source()).collect::<Vec<_>>()
        });

        for tok in toks {
            eprintln!("{:?}", tok.kind);
        }

        timing.add_file(file_summ);
    }

    Ok(())
}
