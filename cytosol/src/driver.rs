use std::path::{Path, PathBuf};

use codespan_reporting::files::SimpleFiles;
use cytosol_hir::{ast_to_hir::Error as AstToHirError, Program};
use cytosol_parser::{ParseError, Token, TokenKind};
use cytosol_syntax::{FileId, FC};

use crate::{
    debug, reporting,
    timing::{FileStage, FileSummary, ProgramStage, TimingReport},
    Config,
};

#[derive(Debug, Clone, Copy)]
pub struct CompileOptions {
    pub dump_tokens: bool,
    pub dump_ast: bool,
}

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

pub struct Driver {
    config: Config,
    timing: TimingReport,
    files: SimpleFiles<FileName, String>,
    file_ids: Vec<FileId>,
}

impl Driver {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            timing: Default::default(),
            files: SimpleFiles::new(),
            file_ids: Default::default(),
        }
    }

    pub fn add_file_from_path(&mut self, path: impl AsRef<Path>) -> std::io::Result<()> {
        let path = path.as_ref();
        let name = FileName::File(path.to_path_buf());

        let source = std::fs::read_to_string(path)?;

        let id = self.files.add(name, source);
        self.file_ids.push(id);

        Ok(())
    }

    pub fn compile(&mut self, options: CompileOptions) -> Result<Program, CompileError> {
        let mut file_asts = vec![];

        for id in &self.file_ids {
            let source_file = self.files.get(*id).unwrap();

            let mut file_sum = FileSummary::new(source_file.name());

            let toks = file_sum.record(FileStage::Lexing, || {
                cytosol_parser::tokenise(*id, source_file.source()).collect::<Vec<_>>()
            });

            if let Some(fc) = first_error_token(&toks) {
                return Err(CompileError::Lexer { unknown_tok_fc: fc });
            }

            if options.dump_tokens {
                debug::dump_tokens(&toks);
            }

            let parse_res = file_sum.record(FileStage::Parsing, || {
                cytosol_parser::parse_file(*id, &toks)
            });

            let ast = parse_res.map_err(CompileError::Parser)?;

            if options.dump_ast {
                debug::dump_ast(&ast);
            }

            self.timing.add_file(file_sum);
            file_asts.push(ast);
        }

        let mut prog = cytosol_hir::Program::new();

        let hir_res = self.timing.record(ProgramStage::AstToHir, || {
            cytosol_hir::ast_to_hir::files_to_hir(&mut prog, &file_asts)
        });

        if let Some(errs) = hir_res {
            return Err(CompileError::AstToHir(errs));
        }

        Ok(prog)
    }

    pub fn report_error(&self, err: &CompileError) {
        match err {
            CompileError::Lexer { unknown_tok_fc } => {
                reporting::report_lexing_error(&self.config, &self.files, *unknown_tok_fc);
            }
            CompileError::Parser(err) => {
                reporting::report_parse_error(&self.config, &self.files, err);
            }
            CompileError::AstToHir(errs) => {
                reporting::report_hir_translate_errors(&self.config, &self.files, errs);
            }
        }
    }

    pub fn timings(&self) -> &TimingReport {
        &self.timing
    }
}

fn first_error_token(toks: &[Token<'_>]) -> Option<FC> {
    for tok in toks {
        if tok.kind == TokenKind::Error {
            return Some(tok.fc);
        }
    }
    None
}
