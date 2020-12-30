use std::cmp::Reverse;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    files::Files,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use cytosol_parser::{ParseError, Token, TokenKind};
use cytosol_syntax::FileId;

fn colour_choice(conf: &crate::Config) -> ColorChoice {
    if conf.no_colour {
        ColorChoice::Never
    } else {
        ColorChoice::Auto
    }
}

pub(crate) fn report_any_lexing_errors<'a>(
    config: &crate::Config,
    files: &'a impl Files<'a, FileId = FileId>,
    toks: &[Token<'a>],
) -> bool {
    let mut found = false;
    for tok in toks {
        if tok.kind == TokenKind::Error {
            let file_source = files.source(tok.fc.file).expect("Invalid file ID");
            let source = &file_source.as_ref()[tok.fc.start..tok.fc.end];

            let label = Label::primary(tok.fc.file, tok.fc.range());
            let diag = Diagnostic::error()
                .with_code("lexer-error")
                .with_message(format!("Invalid token `{}`", source))
                .with_labels(vec![label]);

            emit(config, files, &[diag]);

            found = true;
        }
    }
    found
}

pub(crate) fn report_parse_error<'a>(
    config: &crate::Config,
    files: &'a impl Files<'a, FileId = FileId>,
    err: ParseError,
) {
    let diag = match err {
        ParseError::UnexpectedToken(fc, desc) => {
            let file_source = files.source(fc.file).expect("Invalid file ID");
            let source = &file_source.as_ref()[fc.start..fc.end];

            let (diag_message, label_message) = if let Some(exp) = desc.expected {
                (
                    format!("expected {}, found `{}`", exp, source),
                    format!("expected {}", exp),
                )
            } else {
                (
                    format!("unexpected token `{}`", source),
                    "unexpected token".to_string(),
                )
            };

            let mut labels = vec![Label::primary(fc.file, fc.range()).with_message(label_message)];

            if let Some((fc, ref_desc)) = desc.start {
                labels.push(
                    Label::secondary(fc.file, fc.range())
                        .with_message(format!("{} started here", ref_desc)),
                );
            }

            Diagnostic::error()
                .with_code("parse-error")
                .with_message(diag_message)
                .with_labels(labels)
                .with_notes(vec![format!("error while parsing {}", desc.while_parsing)])
        }
        ParseError::UnexpectedEnd(file, desc) => {
            let (diag_message, label_message) = if let Some(exp) = desc.expected {
                (
                    format!("expected {}, found end of file", exp),
                    format!("expected {}", exp),
                )
            } else {
                (
                    "unexpected end of file".to_string(),
                    "unexpected end of file".to_string(),
                )
            };

            let len = files.source(file).expect("Invalid file ID").as_ref().len();

            let mut labels = vec![Label::primary(file, len..len).with_message(label_message)];

            if let Some((fc, ref_desc)) = desc.start {
                labels.push(
                    Label::secondary(fc.file, fc.range())
                        .with_message(format!("{} started here", ref_desc)),
                );
            }

            Diagnostic::error()
                .with_code("parse-error")
                .with_message(diag_message)
                .with_labels(labels)
                .with_notes(vec![format!("error while parsing {}", desc.while_parsing)])
        }
    };

    emit(config, files, &[diag]);
}

pub(crate) fn report_hir_translate_errors<'a>(
    config: &crate::Config,
    files: &'a impl Files<'a, FileId = FileId>,
    errs: Vec<cytosol_hir::ast_to_hir::Error>,
) {
    let mut diags = vec![];
    for err in errs {
        let diag = match err {
            cytosol_hir::ast_to_hir::Error::RedefinedAtom {
                redef_name,
                orig_name,
            } => {
                let diag_message = format!("redefined atom `{}`", orig_name.1);
                let labels = vec![
                    Label::primary(redef_name.0.file, redef_name.0.range())
                        .with_message(format!("redefined atom `{}` here", redef_name.1)),
                    Label::secondary(orig_name.0.file, orig_name.0.range())
                        .with_message(format!("first definition of atom `{}` here", orig_name.1)),
                ];

                Diagnostic::error()
                    .with_message(diag_message)
                    .with_labels(labels)
            }
            cytosol_hir::ast_to_hir::Error::DuplicateAtomField {
                atom_name,
                field_name,
                first_occurance,
            } => {
                let message = format!(
                    "duplicated atom field `{}` in atom `{}`",
                    field_name.1, atom_name.1
                );
                let labels = vec![
                    Label::primary(field_name.0.file, field_name.0.range())
                        .with_message("duplicate field"),
                    Label::secondary(first_occurance.file, first_occurance.range())
                        .with_message("first occurance of field name"),
                ];
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            cytosol_hir::ast_to_hir::Error::RecursiveAtomDefinitions { defs } => {
                let message = if defs.len() > 1 {
                    "recursive atom types"
                } else {
                    "recursive atom type"
                };

                let mut defs = defs;
                defs.sort_by_key(|k| Reverse(*k));

                let labels = defs
                    .into_iter()
                    .enumerate()
                    .map(|(i, fc)| {
                        let style = if i == 0 {
                            LabelStyle::Primary
                        } else {
                            LabelStyle::Secondary
                        };

                        let message = if i == 0 {
                            "atom has infinite size"
                        } else {
                            "atom is part of a recursive cycle"
                        };

                        Label::new(style, fc.file, fc.range()).with_message(message)
                    })
                    .collect();

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            cytosol_hir::ast_to_hir::Error::UnknownType { name } => {
                let message = format!("unknown type `{}`", name.1);
                let label =
                    Label::primary(name.0.file, name.0.range()).with_message("unknown type");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
        };

        diags.push(diag);
    }

    emit(config, files, &diags);
}

fn emit<'a>(
    conf: &crate::Config,
    files: &'a impl Files<'a, FileId = FileId>,
    diags: &[Diagnostic<FileId>],
) {
    let mut writer = StandardStream::stderr(colour_choice(conf));
    let term_config = Config::default();

    for diag in diags {
        codespan_reporting::term::emit(&mut writer, &term_config, files, &diag).unwrap();
    }
}
