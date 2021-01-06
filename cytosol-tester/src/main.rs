use std::path::PathBuf;

use cytosol::{
    driver::DriverRunner,
    hir::Program,
    runtime::{
        value::Value, CellEnv, CellEnvSummary, ExecutionContext, ProgramContext, RuntimeVars,
    },
};

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

    #[clap(long)]
    no_semantic_analysis: bool,

    #[clap(long)]
    tmp_actually_run: bool,

    file_paths: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Arguments = Arguments::parse();

    let mut prog = cytosol::hir::Program::new();

    let driver = TestDriver::new(args.dump_tokens, args.dump_ast, args.no_semantic_analysis);

    let mut runner = DriverRunner::new(driver);

    for file in &args.file_paths {
        runner.add_file_from_path(file)?;
    }

    if let Err(err) = runner.compile(&mut prog) {
        runner.report_error(&prog, &err, !args.no_colour);
        return Ok(());
    }

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

    if args.tmp_actually_run {
        execute(&prog);
    }

    Ok(())
}

fn execute(prog: &Program) {
    let mut prog_ctx = ProgramContext::new();
    prog_ctx.set_extern_function("print_line", |s: String| {
        println!("{}", s);
    });

    let mut env = CellEnv::default();

    env.records.insert(
        prog.record_by_name("Start").unwrap(),
        vec![Value::Record(vec![])],
    );

    let mut summ = CellEnvSummary::default();
    env.summary(&mut summ);

    let mut exec_ctx = ExecutionContext::default();

    exec_ctx.prepare_execution(prog, &mut summ);

    let mut vars = RuntimeVars::default();

    loop {
        let mut any = false;

        for gene_id in exec_ctx.eligable_genes() {
            vars.clear();

            prog_ctx.run_gene(prog, &mut env, &mut vars, gene_id);
            any = true;
        }

        if !any {
            return;
        }

        env.summary(&mut summ);
        exec_ctx.prepare_execution(prog, &mut summ);
    }
}
