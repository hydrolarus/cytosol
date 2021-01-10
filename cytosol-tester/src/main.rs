use std::path::PathBuf;

use cytosol::{
    driver::DriverRunner,
    hir::Program,
    runtime::{CellEnv, CellEnvSummary, ExecutionPlan, ProgramContext, RuntimeVars},
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

    execute(&prog);

    Ok(())
}

fn execute(prog: &Program) {
    let mut prog_ctx = ProgramContext::new();
    prog_ctx.set_extern_function("print_line", |s: String| println!("{}", s));
    prog_ctx.set_extern_function("print_string", |s: String| print!("{}", s));
    prog_ctx.set_extern_function("print_int", |i: isize| print!("{}", i));

    let mut env = CellEnv::default();

    if let Some(id) = prog.record_by_name("Start") {
        env.add_record(1, id, vec![]);
    }

    let mut summ = CellEnvSummary::default();

    let mut exec_ctx = ExecutionPlan::default();

    let mut vars = RuntimeVars::default();

    loop {
        let mut ran_genes = false;

        env.summary(&mut summ);
        exec_ctx.prepare_gene_execution(prog, &mut summ);

        for gene_id in exec_ctx.eligable_genes() {
            vars.clear();

            prog_ctx.run_gene(prog, &mut env, &mut vars, gene_id);
            ran_genes = true;
        }

        env.summary(&mut summ);
        exec_ctx.prepare_enzyme_execution(prog, &mut summ);

        let ran_enzymes =
            prog_ctx.run_enzymes(prog, &mut env, &mut vars, exec_ctx.eligable_enzymes());

        if !ran_genes && !ran_enzymes {
            return;
        }
    }
}

/*
fn dbg_print_env(prog: &Program, env: &CellEnv) {
    println!("Env:");

    println!("  Records: ");
    for (id, instances) in &env.records {
        let name = &prog[*id].name.1;
        println!("    # {} = {}", name, instances.len());
    }

    println!("  Enzymes: ");
    for (id, num) in &env.enzymes {
        let name = &prog[*id].name.1;
        println!("    # {} = {}", name, num);
    }
    println!();
}

fn dbg_print_exec_plan(prog: &Program, plan: &ExecutionPlan) {
    println!("Env plan:");

    println!("  Genes: ");
    for id in plan.eligable_genes() {
        println!("    {:?}", id);
    }

    println!("  Enzymes: ");
    for (id, n) in plan.eligable_enzymes() {
        let name = &prog[id].name.1;
        println!("    # {} = {}", name, n);
    }
    println!();
}

*/
