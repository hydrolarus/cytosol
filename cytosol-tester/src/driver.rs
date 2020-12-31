use cytosol::{
    driver::{CompileError, Driver, FileName},
    hir::Program,
    syntax::File,
};

use crate::{
    debug,
    perf_track::{FileStage, FileSummary, PerformanceReport, ProgramStage},
};

pub(crate) struct TestDriver {
    pub(crate) perf: PerformanceReport,
    dump_tokens: bool,
    dump_ast: bool,
}

impl TestDriver {
    pub(crate) fn new(dump_tokens: bool, dump_ast: bool) -> Self {
        Self {
            perf: PerformanceReport::default(),
            dump_tokens,
            dump_ast,
        }
    }
}

impl Driver for TestDriver {
    fn process_file(
        &mut self,
        file_name: &FileName,
        file_id: cytosol::syntax::FileId,
        source: &str,
    ) -> Result<File, CompileError> {
        let mut perf = FileSummary::new(file_name);

        let toks = perf
            .record(FileStage::Lexing, || {
                cytosol::parser::tokenise(file_id, source)
            })
            .map_err(|fc| CompileError::Lexer { unknown_tok_fc: fc })?;

        if self.dump_tokens {
            debug::dump_tokens(&toks);
        }

        let ast = perf
            .record(FileStage::Parsing, || {
                cytosol::parser::parse_file(file_id, &toks)
            })
            .map_err(CompileError::Parser)?;

        if self.dump_ast {
            debug::dump_ast(&ast);
        }

        self.perf.add_file(perf);

        Ok(ast)
    }

    fn compile_program(&mut self, files: &[File]) -> Result<Program, CompileError> {
        let mut prog = Program::new();

        self.perf
            .record(ProgramStage::AstToHir, || {
                cytosol::hir::ast_to_hir::files_to_hir(&mut prog, files)
            })
            .map_err(CompileError::AstToHir)?;

        Ok(prog)
    }
}
