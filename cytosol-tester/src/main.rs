use std::path::PathBuf;

use cytosol::driver::DriverRunner;

use clap::Clap;
use driver::TestDriver;

mod debug;
mod driver;
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    let mut runner = DriverRunner::new();

    for file in &args.file_paths {
        runner.add_file_from_path(file)?;
    }

    let mut driver = TestDriver::new(args.dump_tokens, args.dump_ast);

    let _prog = match runner.compile(&mut driver) {
        Ok(prog) => prog,
        Err(err) => {
            runner.report_error(&err, !args.no_colour);
            return Ok(());
        }
    };

    if args.report_timing {
        driver.timing.print_summary(&mut std::io::stdout())?;
    }

    Ok(())
}
