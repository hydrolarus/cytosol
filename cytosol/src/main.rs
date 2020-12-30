use std::path::PathBuf;

use clap::Clap;
use codespan_reporting::files::SimpleFiles;
use timing::{FileStage, FileSummary, ProgramStage, TimingReport};

mod debug;
mod reporting;

mod timing;

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

    #[clap(long)]
    report_timing: bool,

    file_paths: Vec<PathBuf>,
}

pub(crate) struct Config {
    // Do not display colours in the terminal
    pub(crate) no_colour: bool,

    pub(crate) report_timing: bool,
}

impl Config {
    fn from_arguments(args: &Arguments) -> Self {
        Self {
            no_colour: args.no_colour,
            report_timing: args.report_timing,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();
    let config = Config::from_arguments(&args);

    let mut timing = TimingReport::default();

    if args.dump_tokens {
        debug::dump_tokens(&mut timing, &config, &args.file_paths)?;
    } else if args.dump_ast {
        debug::dump_ast(&mut timing, &config, &args.file_paths)?;
    } else {
        run_files(&mut timing, &config, &args.file_paths)?;
    }

    if config.report_timing {
        timing.print_summary(&mut std::io::stdout())?;
    }

    Ok(())
}

fn run_files(
    timing: &mut TimingReport,
    config: &Config,
    paths: &[PathBuf],
) -> Result<(), Box<dyn std::error::Error>> {
    let mut files = SimpleFiles::new();

    let mut parsed_files = vec![];

    for path in paths {
        let mut file_summ = FileSummary::new(path);

        let content = std::fs::read_to_string(path)?;

        let id = files.add(path.to_str().unwrap(), content);

        let toks = file_summ.record(FileStage::Lexing, || {
            cytosol_parser::tokenise(id, files.get(id).unwrap().source()).collect::<Vec<_>>()
        });

        let found_errors = reporting::report_any_lexing_errors(config, &files, &toks);

        if found_errors {
            continue;
        }

        let parse_res =
            file_summ.record(FileStage::Parsing, || cytosol_parser::parse_file(id, &toks));

        let ast = match parse_res {
            Ok(val) => val,
            Err(err) => {
                reporting::report_parse_error(config, &files, err);
                continue;
            }
        };

        timing.add_file(file_summ);
        parsed_files.push(ast);
    }

    // TODO run the file or something
    let mut prog = cytosol_hir::Program::new();

    let hir_res = timing.record(ProgramStage::AstToHir, || {
        cytosol_hir::ast_to_hir::files_to_hir(&mut prog, &parsed_files)
    });

    if let Some(errs) = hir_res {
        reporting::report_hir_translate_errors(config, &files, errs);
    }

    Ok(())
}
