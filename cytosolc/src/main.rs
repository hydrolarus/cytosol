use std::path::PathBuf;

use cytosol::{
    driver::{CompileOptions, Driver},
    Config,
};

use clap::Clap;

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

impl Arguments {
    fn to_config(&self) -> Config {
        Config {
            no_colour: self.no_colour,
        }
    }

    fn to_compile_options(&self) -> CompileOptions {
        CompileOptions {
            dump_tokens: self.dump_tokens,
            dump_ast: self.dump_ast,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();
    let config = args.to_config();
    let compile_options = args.to_compile_options();

    let mut driver = Driver::new(config);

    let mut errs = vec![];

    for file in &args.file_paths {
        if let Err(error) = driver.add_file_from_path(file) {
            errs.push(error);
        }
    }

    match driver.compile(compile_options) {
        Ok(_prog) => {}
        Err(err) => {
            driver.report_error(&err);
        }
    }

    if args.report_timing {
        driver.timings().print_summary(&mut std::io::stdout())?;
    }

    Ok(())
}
