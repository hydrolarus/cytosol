use std::path::{Path, PathBuf};

use codespan_reporting::files::SimpleFiles;
use cytosol_hir::{ast_to_hir::Error as AstToHirError, Program};
use cytosol_parser::ParseError;
use cytosol_syntax::{File, FileId, FC};

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
    Lexer { unknown_tok_fc: FC },
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
            CompileError::Lexer { unknown_tok_fc } => {
                reporting::report_lexing_error(coloured_output, &self.files, *unknown_tok_fc);
            }
            CompileError::Parser(err) => {
                reporting::report_parse_error(coloured_output, &self.files, err);
            }
            CompileError::AstToHir(errs) => {
                reporting::report_hir_translate_errors(coloured_output, &self.files, prog, errs);
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
        let toks = crate::parser::tokenise(file_id, source)
            .map_err(|fc| CompileError::Lexer { unknown_tok_fc: fc })?;

        crate::parser::parse_file(file_id, &toks).map_err(CompileError::Parser)
    }

    fn compile_files(&mut self, prog: &mut Program, files: &[File]) -> Result<(), CompileError> {
        crate::hir::ast_to_hir::files_to_hir(prog, files).map_err(CompileError::AstToHir)?;
        Ok(())
    }
}
