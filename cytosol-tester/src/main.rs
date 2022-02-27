use std::path::PathBuf;

use cytosol::{
    driver::{Driver, DriverExecutionState, DriverRunner},
    hir::Program,
    runtime::CellEnv,
};

use clap::Parser;
use driver::TestDriver;

mod debug;
mod driver;
mod perf_track;

use stats_alloc::{StatsAlloc, INSTRUMENTED_SYSTEM};

#[global_allocator]
pub(crate) static STATS_ALLOC: &StatsAlloc<std::alloc::System> = &INSTRUMENTED_SYSTEM;

#[derive(Parser, Debug)]
#[clap(version = "0.1")]
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
    no_run: bool,

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

    if args.no_run {
        return Ok(());
    }

    execute(&prog, &mut runner);

    Ok(())
}

fn execute<D: Driver>(prog: &Program, runner: &mut DriverRunner<D>) {
    let mut exec_state = DriverExecutionState::default();

    {
        let ctx = exec_state.program_context();
        ctx.set_extern_function("print_line", |s: String| println!("{}", s));
        ctx.set_extern_function("print_string", |s: String| print!("{}", s));
        ctx.set_extern_function("print_int", |i: isize| print!("{}", i));
    }

    let mut env = CellEnv::default();

    if let Some(id) = prog.record_by_name("Start") {
        env.add_record(1, id, vec![]);
    }

    runner.run(prog, &mut exec_state, &mut env, 300);
}

/*
fn dbg_print_env(prog: &Program, env: &CellEnv) {
    println!("Env:");

    println!("  Records: ");
    for (id, instances) in &env.records {
        let name = &prog[*id].name.1;
        println!("    # {} = {}", name, instances.len());
    }

    println!();
}

fn dbg_print_exec_plan(prog: &Program, plan: &ExecutionPlan) {
    println!("Env plan:");

    println!("  Genes: ");
    for id in plan.eligable_genes() {
        println!("    {:?}", id);
    }

    println!("  Rules: ");
    for (id, n) in plan.eligable_rules() {
        let name = &prog[id].name.1;
        println!("    # {} = {}", name, n);
    }
    println!();
}

*/
