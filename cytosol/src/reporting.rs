use std::cmp::Reverse;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    files::Files,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use cytosol_parser::ParseError;
use cytosol_syntax::FileId;

use cytosol_hir::{ast_to_hir::Error, Program};

fn colour_choice(coloured: bool) -> ColorChoice {
    if coloured {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    }
}

pub(crate) fn report_parse_error<'a>(
    coloured: bool,
    files: &'a impl Files<'a, FileId = FileId>,
    err: &ParseError,
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

            let len = files.source(*file).expect("Invalid file ID").as_ref().len();

            let mut labels = vec![Label::primary(*file, len..len).with_message(label_message)];

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

    emit(coloured, files, &[diag]);
}

pub(crate) fn report_hir_translate_errors<'a>(
    coloured: bool,
    files: &'a impl Files<'a, FileId = FileId>,
    prog: &Program,
    errs: &[cytosol_hir::ast_to_hir::Error],
) {
    let mut diags = vec![];
    for err in errs {
        let diag = match err {
            Error::RedefinedItem {
                redef_name,
                orig_name,
            } => {
                let diag_message = format!("redefined item `{}`", orig_name.1);
                let labels = vec![
                    Label::primary(redef_name.0.file, redef_name.0.range())
                        .with_message(format!("redefined item `{}` here", redef_name.1)),
                    Label::secondary(orig_name.0.file, orig_name.0.range())
                        .with_message(format!("first definition of item `{}` here", orig_name.1)),
                ];

                Diagnostic::error()
                    .with_message(diag_message)
                    .with_labels(labels)
            }
            Error::DuplicateRecordField {
                record_name,
                field_name,
                first_occurance,
            } => {
                let message = format!(
                    "duplicated record field `{}` in record `{}`",
                    field_name.1, record_name.1
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
            Error::RecursiveRecordDefinitions { defs } => {
                let message = if defs.len() > 1 {
                    "recursive record types"
                } else {
                    "recursive record type"
                };

                let mut defs = defs.clone();
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
                            "record has infinite size"
                        } else {
                            "record is part of a recursive cycle"
                        };

                        Label::new(style, fc.file, fc.range()).with_message(message)
                    })
                    .collect();

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::UnknownType { name } => {
                let message = format!("unknown type `{}`", name.1);
                let label =
                    Label::primary(name.0.file, name.0.range()).with_message("unknown type");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::UnknownExtern { name } => {
                let message = format!("unknown extern function `{}`", name.1);
                let label = Label::primary(name.0.file, name.0.range())
                    .with_message("unknown extern function");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::InvalidReactantType { name } => {
                let message = format!("type `{}` cannot be used in a reactant list", name.1);
                let label = Label::primary(name.0.file, name.0.range())
                    .with_message("this type cannot be used as a reactant");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::InvalidFactorType { name } => {
                let message = format!(
                    "type `{}` cannot be used in an execution factor list",
                    name.1
                );
                let label = Label::primary(name.0.file, name.0.range())
                    .with_message("this type cannot be used as an execution factor");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::ZeroBind {
                item,
                bind_number,
                type_name,
            } => {
                let message = "used `0` as a bind quantity".to_string();
                let labels = vec![
                    Label::primary(bind_number.file, bind_number.range())
                        .with_message("bind quantity of `0` is not supported"),
                    Label::secondary(item.file, item.range()),
                ];
                let note = format!("use `when [{}] = 0` instead", type_name.1);
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(vec![note])
            }
            Error::RedefinedBuiltinType { redef_name } => {
                let message = format!("redefined builtin type `{}`", redef_name.1);
                let label = Label::primary(redef_name.0.file, redef_name.0.range())
                    .with_message("this type is redefining a built-in type");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::NameRebound {
                item_fc: _,
                name,
                orig_name,
            } => {
                let message = format!("Variable name `{}` bound multiple times", name.1);
                let labels = vec![
                    Label::primary(name.0.file, name.0.range()).with_message("variable bound here"),
                    Label::secondary(orig_name.0.file, orig_name.0.range())
                        .with_message("variable with same name already bound here"),
                ];
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::TypeMismatch {
                fc,
                expected,
                found,
            } => {
                let (expected, _) = prog.type_name(*expected).unwrap();
                let (found, _) = prog.type_name(*found).unwrap();

                let message = format!(
                    "expected expression of type `{}` but found `{}`",
                    expected, found
                );

                let labels = vec![Label::primary(fc.file, fc.range()).with_message(format!(
                    "this has type `{}` but should be of type `{}`",
                    found, expected
                ))];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::TypeMismatchPrefixOp {
                op_fc,
                op: _,
                expr,
                expected_types,
            } => {
                let op_file = files.source(op_fc.file).unwrap();
                let op_src = &op_file.as_ref()[op_fc.range()];

                let expr_fc = prog.expr_fc(*expr).unwrap();
                let expr_ty = prog.expr_type(*expr).unwrap();
                let (expr_ty_name, _) = prog.type_name(expr_ty).unwrap();

                let message = format!(
                    "cannot apply unary operator `{}` to type `{}`",
                    op_src, expr_ty_name
                );

                let expected_types = expected_types
                    .iter()
                    .filter_map(|id| prog.type_name(*id))
                    .map(|(name, _)| format!("- type `{}`", name));

                let labels = vec![
                    Label::secondary(expr_fc.file, expr_fc.range())
                        .with_message(format!("this expression has type `{}`", expr_ty_name)),
                    Label::primary(op_fc.file, op_fc.range()).with_message(format!(
                        "this unary operator is incompatible with type `{}`",
                        expr_ty_name
                    )),
                ];

                let notes = std::iter::once(format!(
                    "the unary operator `{}` is compatible with:",
                    op_src
                ))
                .chain(expected_types)
                .collect::<Vec<_>>();

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(vec![notes.join("\n")])
            }
            Error::TypeMismatchInfixOp {
                op_fc,
                op: _,
                lhs,
                rhs,
                expected_types,
            } => {
                let op_file = files.source(op_fc.file).unwrap();
                let op_src = &op_file.as_ref()[op_fc.range()];

                let lhs_fc = prog.expr_fc(*lhs).unwrap();
                let lhs_ty = prog.expr_type(*lhs).unwrap();
                let (lhs_ty_name, _) = prog.type_name(lhs_ty).unwrap();

                let rhs_fc = prog.expr_fc(*rhs).unwrap();
                let rhs_ty = prog.expr_type(*rhs).unwrap();
                let (rhs_ty_name, _) = prog.type_name(rhs_ty).unwrap();

                let message = format!(
                    "cannot apply infix operator `{}` to types `{}` and `{}`",
                    op_src, lhs_ty_name, rhs_ty_name,
                );

                let expected_types = expected_types
                    .iter()
                    .filter_map(|(a, b)| Some((prog.type_name(*a)?, prog.type_name(*b)?)))
                    .map(|((a, _), (b, _))| format!("- type `{}` and `{}`", a, b));

                let labels = vec![
                    Label::secondary(lhs_fc.file, lhs_fc.range())
                        .with_message(format!("this expression has type `{}`", lhs_ty_name)),
                    Label::secondary(rhs_fc.file, rhs_fc.range())
                        .with_message(format!("this expression has type `{}`", rhs_ty_name)),
                    Label::primary(op_fc.file, op_fc.range()).with_message(format!(
                        "this infix operator is incompatible with types `{}` and `{}`",
                        lhs_ty_name, rhs_ty_name,
                    )),
                ];

                let notes = std::iter::once(format!(
                    "the infix operator `{}` is compatible with:",
                    op_src
                ))
                .chain(expected_types)
                .collect::<Vec<_>>();

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(vec![notes.join("\n")])
            }
            Error::UsingBuiltinTypeAsProduct { product_name } => {
                let message = format!(
                    "builtin type `{}` cannot be used as a product",
                    product_name.1
                );

                let label = Label::primary(product_name.0.file, product_name.0.range())
                    .with_message("builtin type cannot be used a product");
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::ProductMissingRecordField {
                record_name,
                product_fc,
                missing_field,
            } => {
                let message = format!(
                    "product `{}` is missing a `{}` field",
                    record_name.1, missing_field.1
                );

                let labels = vec![
                    Label::primary(product_fc.file, product_fc.range())
                        .with_message("this product is incomplete"),
                    Label::secondary(missing_field.0.file, missing_field.0.range())
                        .with_message("this field is missing"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::ProductDuplicateRecordField {
                record_name,
                duplicate_field,
                original_field,
            } => {
                let message = format!(
                    "duplicated field `{}` on record `{}`",
                    duplicate_field.1, record_name.1
                );

                let labels = vec![
                    Label::primary(duplicate_field.0.file, duplicate_field.0.range())
                        .with_message("this is a duplicate field"),
                    Label::secondary(original_field.0.file, original_field.0.range())
                        .with_message("field already supplied here"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::ProductUnknownRecordField { record_name, field } => {
                let message = format!("unknown field `{}` on record `{}`", field.1, record_name.1);

                let labels = vec![
                    Label::primary(field.0.file, field.0.range()).with_message("unknown field"),
                    Label::secondary(record_name.0.file, record_name.0.range())
                        .with_message("no such field in the definition of the record"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::UndefinedVariable { name, in_scope: _ } => {
                // TODO suggest similar names in scope
                let message = format!("undefined variable `{}`", name.1);

                let label =
                    Label::primary(name.0.file, name.0.range()).with_message("undefined variable");

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![label])
            }
            Error::FieldIndexOnNonRecord {
                field_name,
                base_type,
            } => {
                let (type_name, type_fc) = prog.type_name(*base_type).unwrap();

                let message = format!(
                    "tried to access field `{}` on non-record type `{}`",
                    field_name.1, type_name,
                );

                let mut labels = vec![Label::primary(field_name.0.file, field_name.0.range())
                    .with_message(format!("no such field on non-record type `{}`", type_name))];

                if let Some(fc) = type_fc {
                    labels.push(Label::secondary(fc.file, fc.range()).with_message(format!(
                        "this non-record type does not have the field `{}`",
                        field_name.1
                    )));
                }

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::InvalidRecordFieldIndex {
                field_name,
                record_name,
            } => {
                let message = format!(
                    "invalid field `{}` on record type `{}`",
                    field_name.1, record_name.1
                );

                let labels = vec![
                    Label::primary(field_name.0.file, field_name.0.range())
                        .with_message(format!("no such field on type `{}`", record_name.1)),
                    Label::secondary(record_name.0.file, record_name.0.range())
                        .with_message("field not present in this record"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::ExternDuplicateParameterName {
                ext_name,
                duplicate_param,
                original_param,
            } => {
                let message = format!(
                    "duplicate parameter `{}` on extern function `{}`",
                    duplicate_param.1, ext_name.1
                );

                let labels = vec![
                    Label::primary(duplicate_param.0.file, duplicate_param.0.range())
                        .with_message("duplicate parameter name"),
                    Label::secondary(original_param.0.file, original_param.0.range())
                        .with_message("parameter with the same name declared here before"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::CallDuplicateParameter {
                ext_name,
                duplicate_param,
                original_param,
            } => {
                let message = format!(
                    "duplicated parameter `{}` in call to `{}`",
                    duplicate_param.1, ext_name.1
                );

                let labels = vec![
                    Label::primary(duplicate_param.0.file, duplicate_param.0.range())
                        .with_message("this is a duplicate parameter"),
                    Label::secondary(original_param.0.file, original_param.0.range())
                        .with_message("paremeter already supplied here"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::CallMissingParameter {
                ext_name,
                call_fc,
                missing_param,
            } => {
                let message = format!(
                    "call to `{}` is missing the `{}` parameter",
                    ext_name.1, missing_param.1
                );

                let labels = vec![
                    Label::primary(call_fc.file, call_fc.range())
                        .with_message("this call statement is incomplete"),
                    Label::secondary(missing_param.0.file, missing_param.0.range())
                        .with_message("this parameter is missing"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::CallUnknownParameter {
                ext_name,
                parameter,
            } => {
                let message = format!(
                    "unknown parameter `{}` in call to `{}`",
                    parameter.1, ext_name.1
                );

                let labels = vec![
                    Label::primary(parameter.0.file, parameter.0.range())
                        .with_message("unknown parameter"),
                    Label::secondary(ext_name.0.file, ext_name.0.range())
                        .with_message("no such parameter in the definition of the extern function"),
                ];

                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::ConcentrationOfNonRecordType { fc: _, type_name } => {
                let message = format!("concentration of type {} cannot be measured", type_name.1);
                let labels = vec![Label::primary(type_name.0.file, type_name.0.range())
                    .with_message("non record type used in concentration expression")];
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
            Error::WhenClauseMustBeOfTypeBool { expr, type_id } => {
                let expr_fc = prog.exprs_fc[expr];
                let (type_name, _) = prog.type_name(*type_id).unwrap();
                let message = "where clause must be of type `bool`".to_string();
                let labels = vec![Label::primary(expr_fc.file, expr_fc.range())
                    .with_message(format!("this expression has type `{}`", type_name))];
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
            }
        };

        diags.push(diag);
    }

    emit(coloured, files, &diags);
}

fn emit<'a>(
    coloured: bool,
    files: &'a impl Files<'a, FileId = FileId>,
    diags: &[Diagnostic<FileId>],
) {
    let mut writer = StandardStream::stderr(colour_choice(coloured));
    let term_config = Config::default();

    for diag in diags {
        codespan_reporting::term::emit(&mut writer, &term_config, files, &diag).unwrap();
    }
}
