use std::path::PathBuf;

use cytosol::driver::DriverRunner;

use clap::Clap;
use driver::TestDriver;

mod debug;
mod driver;
mod perf_track;

use stats_alloc::{StatsAlloc, INSTRUMENTED_SYSTEM};

#[global_allocator]
pub(crate) static STATS_ALLOC: &StatsAlloc<std::alloc::System> = &INSTRUMENTED_SYSTEM;

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
    perf_report: bool,

    #[clap(long)]
    per_file_perf_report: bool,

    file_paths: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    let driver = TestDriver::new(args.dump_tokens, args.dump_ast);

    let mut runner = DriverRunner::new(driver);

    for file in &args.file_paths {
        runner.add_file_from_path(file)?;
    }

    let _prog = match runner.compile() {
        Ok(prog) => prog,
        Err(err) => {
            runner.report_error(&err, !args.no_colour);
            return Ok(());
        }
    };

    if args.perf_report {
        runner
            .driver()
            .perf
            .print_perf_report(&mut std::io::stdout())?;
    }

    if args.per_file_perf_report {
        runner
            .driver()
            .perf
            .print_per_file_perf_report(&mut std::io::stdout())?;
    }

    Ok(())
}
