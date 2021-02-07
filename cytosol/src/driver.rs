use std::path::{Path, PathBuf};

use codespan_reporting::files::SimpleFiles;
use cytosol_hir::{ast_to_hir::Error as AstToHirError, Program};
use cytosol_parser::ParseError;
use cytosol_runtime::{
    run_gene, run_rules, CellEnv, CellEnvSummary, ExecutionPlan, ProgramContext, RuntimeVars,
};
use cytosol_syntax::{File, FileId};

use crate::reporting;

#[derive(Debug, Clone)]
pub enum FileName {
    Virtual(String),
    File(PathBuf),
}

impl std::fmt::Display for FileName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileName::Virtual(s) => f.write_str(s),
            FileName::File(s) => f.write_fmt(format_args!("{}", s.display())),
        }
    }
}

pub enum CompileError {
    Parser(ParseError),
    AstToHir(Vec<AstToHirError>),
}

pub trait Driver {
    fn process_file(
        &mut self,
        file_name: &FileName,
        file_id: FileId,
        source: &str,
    ) -> Result<File, CompileError>;

    fn compile_files(&mut self, prog: &mut Program, files: &[File]) -> Result<(), CompileError>;

    fn execution_iteration(
        &mut self,
        prog: &Program,
        exec_state: &mut DriverExecutionState,
        env: &mut CellEnv,
    ) -> RunResult;
}

pub struct DriverRunner<D: Driver = DefaultDriver> {
    driver: D,
    files: SimpleFiles<FileName, String>,
    file_ids: Vec<FileId>,
    latest_file_ids: Vec<FileId>,
}

impl Default for DriverRunner {
    fn default() -> Self {
        Self::new(DefaultDriver::default())
    }
}

impl<D: Driver> DriverRunner<D> {
    pub fn new(driver: D) -> Self {
        Self {
            driver,
            files: SimpleFiles::new(),
            file_ids: Default::default(),
            latest_file_ids: vec![],
        }
    }

    pub fn add_file_from_path(&mut self, path: impl AsRef<Path>) -> std::io::Result<()> {
        let path = path.as_ref();
        let name = FileName::File(path.to_path_buf());

        let source = std::fs::read_to_string(path)?;

        let id = self.files.add(name, source);
        self.file_ids.push(id);
        self.latest_file_ids.push(id);

        Ok(())
    }

    pub fn add_file_from_string(&mut self, name: impl Into<String>, source: String) {
        let name = FileName::Virtual(name.into());

        let id = self.files.add(name, source);
        self.file_ids.push(id);
        self.latest_file_ids.push(id);
    }

    pub fn compile(&mut self, prog: &mut Program) -> Result<(), CompileError> {
        let mut file_asts = vec![];

        for id in self.latest_file_ids.drain(..) {
            let source_file = self.files.get(id).unwrap();

            let ast = self
                .driver
                .process_file(source_file.name(), id, source_file.source())?;

            file_asts.push(ast);
        }

        self.driver.compile_files(prog, &file_asts)?;

        Ok(())
    }

    pub fn report_error(&self, prog: &Program, err: &CompileError, coloured_output: bool) {
        match err {
            CompileError::Parser(err) => {
                reporting::report_parse_error(coloured_output, &self.files, err);
            }
            CompileError::AstToHir(errs) => {
                reporting::report_hir_translate_errors(coloured_output, &self.files, prog, errs);
            }
        }
    }

    pub fn run_single_iteration(
        &mut self,
        prog: &Program,
        exec_state: &mut DriverExecutionState,
        env: &mut CellEnv,
    ) -> RunResult {
        self.driver.execution_iteration(prog, exec_state, env)
    }

    pub fn run(
        &mut self,
        prog: &Program,
        exec_state: &mut DriverExecutionState,
        env: &mut CellEnv,
        iter_bound: impl Into<Option<usize>>,
    ) {
        let bound = iter_bound.into();

        if let Some(iterations) = bound {
            for _ in 0..iterations {
                let res = self.run_single_iteration(prog, exec_state, env);
                if res == RunResult::NoProgress {
                    return;
                }
            }
        } else {
            loop {
                let res = self.run_single_iteration(prog, exec_state, env);
                if res == RunResult::NoProgress {
                    return;
                }
            }
        }
    }

    pub fn driver(&self) -> &D {
        &self.driver
    }

    pub fn driver_mut(&mut self) -> &mut D {
        &mut self.driver
    }

    pub fn into_driver(self) -> D {
        self.driver
    }
}

#[derive(Default, Debug)]
pub struct DefaultDriver;

impl Driver for DefaultDriver {
    fn process_file(
        &mut self,
        _file_name: &FileName,
        file_id: FileId,
        source: &str,
    ) -> Result<File, CompileError> {
        let toks = crate::parser::tokenise(file_id, source);

        crate::parser::parse_file(file_id, toks).map_err(CompileError::Parser)
    }

    fn compile_files(&mut self, prog: &mut Program, files: &[File]) -> Result<(), CompileError> {
        crate::hir::ast_to_hir::files_to_hir(prog, files).map_err(CompileError::AstToHir)?;
        Ok(())
    }

    fn execution_iteration(
        &mut self,
        prog: &Program,
        exec_state: &mut DriverExecutionState,
        env: &mut CellEnv,
    ) -> RunResult {
        let gene_res = exec_state.run_gene_stage(prog, env);
        let rule_res = exec_state.run_rule_stage(prog, env);

        gene_res.and_then(rule_res)
    }
}

#[derive(Default)]
pub struct DriverExecutionState {
    prog_ctx: ProgramContext,
    cell_env_summ: CellEnvSummary,
    exec_plan: ExecutionPlan,
    runtime_vars: RuntimeVars,
}

impl DriverExecutionState {
    pub fn program_context(&mut self) -> &mut ProgramContext {
        &mut self.prog_ctx
    }

    pub fn run_gene_stage(&mut self, prog: &Program, env: &mut CellEnv) -> RunResult {
        env.summary(&mut self.cell_env_summ);
        self.exec_plan
            .prepare_gene_execution(prog, &mut self.cell_env_summ);

        let mut ran_any_genes = false;
        for gene_id in self.exec_plan.eligable_genes() {
            self.runtime_vars.clear();

            run_gene(
                &mut self.prog_ctx,
                prog,
                env,
                &mut self.runtime_vars,
                gene_id,
            );
            ran_any_genes = true;
        }

        if ran_any_genes {
            RunResult::MadeProgress
        } else {
            RunResult::NoProgress
        }
    }

    pub fn run_rule_stage(&mut self, prog: &Program, env: &mut CellEnv) -> RunResult {
        env.summary(&mut self.cell_env_summ);
        self.exec_plan
            .prepare_rule_execution(prog, &mut self.cell_env_summ);

        let ran_any_rules = run_rules(
            prog,
            env,
            &mut self.runtime_vars,
            self.exec_plan.eligable_rules(),
        );

        if ran_any_rules {
            RunResult::MadeProgress
        } else {
            RunResult::NoProgress
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunResult {
    MadeProgress,
    NoProgress,
}

impl RunResult {
    pub fn and_then(self, other: Self) -> Self {
        match self {
            RunResult::MadeProgress => RunResult::MadeProgress,
            RunResult::NoProgress => other,
        }
    }
}
