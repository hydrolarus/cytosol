use std::collections::{BTreeMap, HashMap};

use crate::{types::*, Program};

use ast::{HasFC, Identifier, InfixOperator, PrefixOperator, FC};
use cytosol_syntax as ast;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Item {} was redefined", .redef_name.1)]
    RedefinedItem {
        redef_name: Identifier,
        orig_name: Identifier,
    },

    #[error("Builtin type {} was redefined", .redef_name.1)]
    RedefinedBuiltinType { redef_name: Identifier },

    #[error("Record {} has duplicated field {}", .record_name.1, .field_name.1)]
    DuplicateRecordField {
        record_name: ast::Identifier,
        field_name: ast::Identifier,
        first_occurance: FC,
    },

    #[error("Recursive record definitions")]
    RecursiveRecordDefinitions { defs: Vec<FC> },

    #[error("Name {} rebound", .name.1)]
    NameRebound {
        item_fc: FC,
        name: Identifier,
        orig_name: Identifier,
    },

    #[error("Type {} unknown", .name.1)]
    UnknownType { name: Identifier },

    #[error("Extern function {} does not exist", .name.1)]
    UnknownExtern { name: Identifier },

    #[error("Type {} can't be used as a reactant", .name.1)]
    InvalidReactantType { name: Identifier },

    #[error("Type {} can't be used as an execution factor", .name.1)]
    InvalidFactorType { name: Identifier },

    #[error("Type mismatch")]
    TypeMismatch {
        fc: FC,
        expected: TypeId,
        found: TypeId,
    },

    #[error("Type mismatch on prefix op {:?}", .op)]
    TypeMismatchPrefixOp {
        op_fc: FC,
        op: PrefixOperator,
        expr: ExpressionId,
        expected_types: Vec<TypeId>,
    },

    #[error("Type mismatch on infix op {:?}", .op)]
    TypeMismatchInfixOp {
        op_fc: FC,
        op: InfixOperator,
        lhs: ExpressionId,
        rhs: ExpressionId,
        expected_types: Vec<(TypeId, TypeId)>,
    },

    #[error("Using builtin type {} as a product", .product_name.1)]
    UsingBuiltinTypeAsProduct { product_name: Identifier },

    #[error("Product is missing the {} field", .missing_field.1)]
    ProductMissingRecordField {
        record_name: Identifier,
        product_fc: FC,
        missing_field: Identifier,
    },

    #[error("Duplicated field {} on record {}", .duplicate_field.1, .record_name.1)]
    ProductDuplicateRecordField {
        record_name: Identifier,
        duplicate_field: Identifier,
        original_field: Identifier,
    },

    #[error("Unknown field {} on record {}", .field.1, record_name.1)]
    ProductUnknownRecordField {
        record_name: Identifier,
        field: Identifier,
    },

    #[error("Undefined variable {}", .name.1)]
    UndefinedVariable {
        name: Identifier,
        in_scope: Vec<Identifier>,
    },

    #[error("Field index `.{}` on non record type", .field_name.1)]
    FieldIndexOnNonRecord {
        field_name: Identifier,
        base_type: TypeId,
    },

    #[error("Invalid field index `.{}` on type {}", .field_name.1, .record_name.1)]
    InvalidRecordFieldIndex {
        field_name: Identifier,
        record_name: Identifier,
    },

    #[error("Duplicate parameter {} on extern function {}", .duplicate_param.1, .ext_name.1)]
    ExternDuplicateParameterName {
        ext_name: Identifier,
        duplicate_param: Identifier,
        original_param: Identifier,
    },

    #[error("Duplicate parameter `{}` in call to `{}`", .duplicate_param.1, .ext_name.1)]
    CallDuplicateParameter {
        ext_name: Identifier,
        duplicate_param: Identifier,
        original_param: Identifier,
    },

    #[error("Missing parameter `{}` in call to `{}`", .missing_param.1, .ext_name.1)]
    CallMissingParameter {
        ext_name: Identifier,
        call_fc: FC,
        missing_param: Identifier,
    },

    #[error("Unknown parameter `{}` in call to `{}`", .parameter.1, .ext_name.1)]
    CallUnknownParameter {
        ext_name: Identifier,
        parameter: Identifier,
    },
}

pub fn files_to_hir(prog: &mut Program, files: &[ast::File]) -> Result<(), Vec<Error>> {
    let mut t = Translator {
        prog,
        errors: Default::default(),
    };

    let _ = t.translate_files(files);

    if t.errors.is_empty() {
        Ok(())
    } else {
        Err(t.errors)
    }
}

type VariableMap<'a> = HashMap<&'a str, (Identifier, TypeId)>;

struct Translator<'a> {
    prog: &'a mut Program,
    errors: Vec<Error>,
}

impl Translator<'_> {
    fn add_error(&mut self, err: Error) {
        self.errors.push(err);
    }

    fn translate_files(&mut self, files: &[ast::File]) {
        self.setup_records(files);

        self.setup_rules(files);

        self.setup_externs(files);

        self.setup_genes(files);
    }

    fn setup_records(&mut self, files: &[ast::File]) {
        // sort by dependency
        // Because there are currently no optional types, so recursive
        // types are not allowed.
        let mut name_to_edge = HashMap::new();
        let mut name_to_fc = HashMap::new();

        type RecordIdx = (usize, usize);

        let mut g = petgraph::Graph::<RecordIdx, RecordIdx>::new();

        // generate all indices first
        for (f, file) in files.iter().enumerate() {
            for (a, record) in file.records.iter().enumerate() {
                if let Some(prev) = name_to_fc.insert(&record.name.1, &record.name) {
                    self.add_error(Error::RedefinedItem {
                        orig_name: prev.clone(),
                        redef_name: record.name.clone(),
                    });
                    continue;
                }

                let edge = g.add_node((f, a));
                name_to_edge.insert(record.name.1.clone(), edge);
            }
        }

        // add all edges
        for file in files {
            for record in &file.records {
                let self_id = &name_to_edge[&record.name.1];

                let edges = record
                    .fields
                    .iter()
                    .filter_map(|(_, ast::Type::Named(n))| name_to_edge.get(&n.1))
                    .map(|other| (*self_id, *other));

                g.extend_with_edges(edges);
            }
        }

        let deps = petgraph::algo::tarjan_scc(&g);

        for group in deps {
            if group.len() > 1 {
                // found a recursive set of record definitions ⇒ error
                let defs: Vec<FC> = group
                    .into_iter()
                    .map(|i| g[i])
                    .map(|(f, a)| files[f].records[a].name.0)
                    .collect();
                self.add_error(Error::RecursiveRecordDefinitions { defs });
                continue;
            }

            debug_assert_eq!(group.len(), 1);

            let (f, a) = g[group[0]];
            let record: &ast::Record = &files[f].records[a];

            // check if record depends on itself
            if record
                .fields
                .iter()
                .any(|(_, ast::Type::Named(n))| n.1 == record.name.1)
            {
                self.add_error(Error::RecursiveRecordDefinitions {
                    defs: vec![record.name.0],
                });
                continue;
            }

            // record is not self-recursive, nice!

            let mut hir_record = Record {
                name: record.name.clone(),
                field_names: vec![],
                fields: vec![],
            };

            for (name, ty) in &record.fields {
                if let Some(prev) = hir_record.field_names.iter().find(|f| f.1 == name.1) {
                    self.add_error(Error::DuplicateRecordField {
                        record_name: record.name.clone(),
                        field_name: name.clone(),
                        first_occurance: prev.0,
                    });
                }

                let ast::Type::Named(n) = ty;

                if let Some(id) = self.prog.type_by_name(&n.1) {
                    hir_record.fields.push(id);
                    hir_record.field_names.push(name.clone());
                } else {
                    self.add_error(Error::UnknownType { name: n.clone() });
                }
            }

            if let Some(id) = self.prog.add_record(record.fc, hir_record) {
                self.prog.add_type(Type::Record(id));
            } else {
                let type_id = self.prog.type_by_name(&record.name.1).unwrap();
                let prev_name = if let Some(n) = self.prog.type_ident(type_id) {
                    n.clone()
                } else {
                    self.add_error(Error::RedefinedBuiltinType {
                        redef_name: record.name.clone(),
                    });
                    continue;
                };
                self.add_error(Error::RedefinedItem {
                    orig_name: prev_name,
                    redef_name: record.name.clone(),
                });
            }
        }
    }

    // NOTE: records must be setup first! Otherwise they can't be used as products or
    // reactant types
    fn setup_rules(&mut self, files: &[ast::File]) {
        // after they have all been added their binds are filled
        for file in files {
            for rule in &file.rules {
                let mut binds = vec![];
                let mut bound_vars = VariableMap::new();

                for reactant in &rule.reactants {
                    let bind_attr = match &reactant.attr {
                        Some(ast::BindingAttribute::Quantity(_, n)) => {
                            if *n == 0 {
                                Bind::None
                            } else {
                                Bind::Quantity(*n)
                            }
                        }
                        Some(ast::BindingAttribute::Name(name)) => {
                            let ty = if let Some(ty) = self.prog.type_by_name(&reactant.name.1) {
                                ty
                            } else {
                                self.add_error(Error::UnknownType {
                                    name: reactant.name.clone(),
                                });
                                continue;
                            };
                            if let Some((prev, _)) = bound_vars.insert(&name.1, (name.clone(), ty))
                            {
                                self.add_error(Error::NameRebound {
                                    item_fc: rule.fc,
                                    name: name.clone(),
                                    orig_name: prev,
                                });
                                continue;
                            }

                            Bind::Named(name.clone())
                        }
                        None => Bind::Quantity(1),
                    };

                    if let Some(record_id) = self.prog.record_by_name(&reactant.name.1) {
                        binds.push((bind_attr, record_id))
                    } else {
                        self.add_error(Error::InvalidReactantType {
                            name: reactant.name.clone(),
                        });
                    }
                }

                let products = rule
                    .products
                    .iter()
                    .flat_map(|p| self.translate_product(&bound_vars, p))
                    .collect();

                let _ = self.prog.add_rule(rule.fc, Rule { binds, products });
            }
        }
    }

    fn setup_externs(&mut self, files: &[ast::File]) {
        let mut fields = BTreeMap::<&str, Identifier>::new();

        for file in files {
            for ext in &file.externs {
                fields.clear();

                let mut hir_ext = Extern {
                    name: ext.name.clone(),
                    parameters: vec![],
                    parameter_names: vec![],
                };

                for (name, ty) in &ext.parameters {
                    let ast::Type::Named(type_name) = ty;

                    let ty_id = if let Some(id) = self.prog.type_by_name(&type_name.1) {
                        id
                    } else {
                        self.add_error(Error::UnknownType {
                            name: type_name.clone(),
                        });
                        continue;
                    };

                    if let Some(prev) = fields.insert(name.1.as_str(), name.clone()) {
                        self.add_error(Error::ExternDuplicateParameterName {
                            ext_name: ext.name.clone(),
                            duplicate_param: name.clone(),
                            original_param: prev,
                        });
                        continue;
                    }

                    hir_ext.parameter_names.push(name.clone());
                    hir_ext.parameters.push(ty_id);
                }

                if self.prog.add_extern(ext.fc, hir_ext).is_none() {
                    let redef_id = self.prog.extern_by_name(&ext.name.1).unwrap();
                    let redef_ext = &self.prog[redef_id];

                    let err = Error::RedefinedItem {
                        orig_name: redef_ext.name.clone(),
                        redef_name: ext.name.clone(),
                    };
                    self.add_error(err);
                    continue;
                }
            }
        }
    }

    fn setup_genes(&mut self, files: &[ast::File]) {
        for file in files {
            for gene in &file.genes {
                let mut binds = vec![];
                let mut bound_vars = VariableMap::new();

                for factor in &gene.factors {
                    let bind_attr = match &factor.attr {
                        Some(ast::BindingAttribute::Quantity(_, n)) => {
                            if *n == 0 {
                                Bind::None
                            } else {
                                Bind::Quantity(*n)
                            }
                        }
                        Some(ast::BindingAttribute::Name(name)) => {
                            let ty = if let Some(ty) = self.prog.type_by_name(&factor.name.1) {
                                ty
                            } else {
                                self.add_error(Error::UnknownType {
                                    name: factor.name.clone(),
                                });
                                continue;
                            };
                            if let Some((prev, _)) = bound_vars.insert(&name.1, (name.clone(), ty))
                            {
                                self.add_error(Error::NameRebound {
                                    item_fc: gene.fc,
                                    name: name.clone(),
                                    orig_name: prev,
                                });
                                continue;
                            }

                            Bind::Named(name.clone())
                        }
                        None => Bind::Quantity(1),
                    };

                    if let Some(record_id) = self.prog.record_by_name(&factor.name.1) {
                        binds.push((bind_attr, record_id))
                    } else {
                        self.add_error(Error::InvalidFactorType {
                            name: factor.name.clone(),
                        });
                    }
                }

                let body = gene
                    .body
                    .iter()
                    .filter_map(|s| self.translate_gene_statement(&bound_vars, s))
                    .collect();

                let hir_gene = Gene { binds, body };

                self.prog.add_gene(gene.fc(), hir_gene);
            }
        }
    }

    fn translate_product(&mut self, vars: &VariableMap, product: &ast::Product) -> Option<Product> {
        let type_id = if let Some(id) = self.prog.type_by_name(&product.name.1) {
            id
        } else {
            self.add_error(Error::UnknownType {
                name: product.name.clone(),
            });
            return None;
        };
        let ty = self.prog.typ(type_id).unwrap();
        match ty {
            Type::Int | Type::String => {
                self.add_error(Error::UsingBuiltinTypeAsProduct {
                    product_name: product.name.clone(),
                });
                None
            }
            Type::Record(id) => {
                use std::collections::btree_map::Entry;

                let mut errs = vec![];

                let id = *id;

                let record = self.prog[id].clone();

                // already ordered/sorted by field name
                let mut args = vec![];
                let mut call_fields = BTreeMap::new();

                for (ident, expr) in &product.fields {
                    match call_fields.entry(ident.1.as_str()) {
                        Entry::Vacant(e) => {
                            e.insert((ident, expr));
                        }
                        Entry::Occupied(e) => {
                            let (orig_ident, _) = e.get();
                            errs.push(Error::ProductDuplicateRecordField {
                                record_name: record.name.clone(),
                                duplicate_field: ident.clone(),
                                original_field: (*orig_ident).clone(),
                            });
                            continue;
                        }
                    }
                }

                for (field_name, field_ty) in record.field_names.iter().zip(&record.fields) {
                    // find the field inside the product call

                    if let Some((_, expr)) = call_fields.remove(&field_name.1.as_str()) {
                        if let Some(expr_id) = self.translate_expr(vars, expr) {
                            let ty = self.prog.expr_type(expr_id).unwrap();
                            if field_ty != &ty {
                                errs.push(Error::TypeMismatch {
                                    fc: expr.fc(),
                                    expected: *field_ty,
                                    found: ty,
                                });
                                continue;
                            }
                            args.push(expr_id);
                        } else {
                            continue;
                        }
                    } else {
                        errs.push(Error::ProductMissingRecordField {
                            record_name: record.name.clone(),
                            product_fc: product.fc,
                            missing_field: field_name.clone(),
                        });
                    }
                }

                for (_, (ident, _)) in call_fields {
                    errs.push(Error::ProductUnknownRecordField {
                        record_name: record.name.clone(),
                        field: ident.clone(),
                    });
                }

                self.errors.extend(errs);

                Some(Product {
                    quantity: product.quantity.map(|(_, n)| n).unwrap_or(1),
                    record: id,
                    arguments: args,
                })
            }
        }
    }

    fn translate_gene_statement(
        &mut self,
        vars: &VariableMap,
        stmt: &ast::GeneStatement,
    ) -> Option<GeneStatementId> {
        let fc = stmt.fc();
        match stmt {
            ast::GeneStatement::Call {
                fc: _,
                name,
                arguments,
            } => {
                use std::collections::btree_map::Entry;

                let ext_id = if let Some(id) = self.prog.extern_by_name(&name.1) {
                    id
                } else {
                    self.add_error(Error::UnknownExtern { name: name.clone() });
                    return None;
                };

                let ext = self.prog[ext_id].clone();

                let mut errs = vec![];

                // already ordered/sorted by parameter name
                let mut args = vec![];
                let mut call_params = BTreeMap::new();

                for (ident, expr) in arguments {
                    match call_params.entry(ident.1.as_str()) {
                        Entry::Vacant(e) => {
                            e.insert((ident, expr));
                        }
                        Entry::Occupied(e) => {
                            let (orig_ident, _) = e.get();
                            errs.push(Error::CallDuplicateParameter {
                                ext_name: ext.name.clone(),
                                duplicate_param: ident.clone(),
                                original_param: (*orig_ident).clone(),
                            });
                            continue;
                        }
                    }
                }

                for (param_name, param_ty) in ext.parameter_names.iter().zip(&ext.parameters) {
                    // find the parameter inside the extern function def

                    if let Some((_, expr)) = call_params.remove(&param_name.1.as_str()) {
                        if let Some(expr_id) = self.translate_expr(vars, expr) {
                            let ty = self.prog.expr_type(expr_id).unwrap();
                            if param_ty != &ty {
                                errs.push(Error::TypeMismatch {
                                    fc: expr.fc(),
                                    expected: *param_ty,
                                    found: ty,
                                });
                                continue;
                            }
                            args.push(expr_id);
                        } else {
                            continue;
                        }
                    } else {
                        errs.push(Error::CallMissingParameter {
                            ext_name: ext.name.clone(),
                            call_fc: fc,
                            missing_param: param_name.clone(),
                        });
                    }
                }

                for (_, (ident, _)) in call_params {
                    errs.push(Error::CallUnknownParameter {
                        ext_name: ext.name.clone(),
                        parameter: ident.clone(),
                    });
                }

                self.errors.extend(errs);

                let stmt = GeneStatement::Call {
                    ext: ext_id,
                    arguments: args,
                };

                let id = self.prog.add_gene_statement(fc, stmt);

                Some(id)
            }
            ast::GeneStatement::Express(_, prod) => {
                let product = self.translate_product(vars, prod)?;
                let id = self
                    .prog
                    .add_gene_statement(fc, GeneStatement::Express(product));
                Some(id)
            }
        }
    }

    fn translate_expr(
        &mut self,
        vars: &VariableMap,
        expr: &ast::Expression,
    ) -> Option<ExpressionId> {
        let fc = expr.fc();
        let (expr, ty) = match expr {
            ast::Expression::Literal(ast::Literal::Integer(_, n)) => {
                let expr = Expression::IntegerLiteral(*n);
                let ty = self.prog.type_int_id;
                (expr, ty)
            }
            ast::Expression::Literal(ast::Literal::String(_, s)) => {
                let expr = Expression::StringLiteral(s.clone());
                let ty = self.prog.type_string_id;
                (expr, ty)
            }
            ast::Expression::Variable(ident) => {
                //
                if let Some((_, ty)) = vars.get(&ident.1.as_ref()) {
                    (Expression::Variable(ident.clone()), *ty)
                } else {
                    let in_scope = vars.values().map(|(i, _)| i.clone()).collect();
                    self.add_error(Error::UndefinedVariable {
                        name: ident.clone(),
                        in_scope,
                    });
                    return None;
                }
            }
            ast::Expression::FieldAccess { base, field_name } => {
                //
                let base_id = self.translate_expr(vars, base)?;
                let base_type = self.prog.expr_type(base_id)?;
                let ty = &self.prog[base_type];
                match ty {
                    Type::Record(record_id) => {
                        let record = &self.prog[*record_id];
                        if let Some((idx, _)) = record
                            .field_names
                            .iter()
                            .enumerate()
                            .find(|(_, name)| name.1 == field_name.1)
                        {
                            let expr = Expression::FieldAccess {
                                base: base_id,
                                field: idx,
                            };
                            let ty = record.fields[idx];
                            (expr, ty)
                        } else {
                            let err = Error::InvalidRecordFieldIndex {
                                record_name: record.name.clone(),
                                field_name: field_name.clone(),
                            };
                            self.add_error(err);
                            return None;
                        }
                    }
                    _ => {
                        self.errors.push(Error::FieldIndexOnNonRecord {
                            field_name: field_name.clone(),
                            base_type,
                        });
                        return None;
                    }
                }
            }
            ast::Expression::PrefixOp {
                op: (op_fc, op),
                expr,
            } => {
                let expr_id = self.translate_expr(vars, expr)?;
                let expr_ty = self.prog.expr_type(expr_id)?;

                match op {
                    ast::PrefixOperator::Neg => {
                        let expected_types = vec![self.prog.type_int_id];
                        if !expected_types.contains(&expr_ty) {
                            self.add_error(Error::TypeMismatchPrefixOp {
                                op_fc: *op_fc,
                                op: *op,
                                expr: expr_id,
                                expected_types,
                            });
                            return None;
                        }
                        debug_assert_eq!(expr_ty, self.prog.type_int_id);
                        let ty = self.prog.type_int_id;
                        let expr = Expression::PrefixOp {
                            op: *op,
                            expr: expr_id,
                        };
                        (expr, ty)
                    }
                }
            }
            ast::Expression::InfixOp {
                op: (op_fc, op),
                args,
            } => {
                let lhs_id = self.translate_expr(vars, &args[0])?;
                let rhs_id = self.translate_expr(vars, &args[1])?;

                let lhs_type = self.prog.expr_type(lhs_id)?;
                let rhs_type = self.prog.expr_type(rhs_id)?;

                let operator_types = match op {
                    InfixOperator::Add => {
                        vec![
                            (
                                (self.prog.type_int_id, self.prog.type_int_id),
                                self.prog.type_int_id,
                            ),
                            (
                                (self.prog.type_string_id, self.prog.type_string_id),
                                self.prog.type_string_id,
                            ),
                        ]
                    }
                    InfixOperator::Sub => vec![(
                        (self.prog.type_int_id, self.prog.type_int_id),
                        self.prog.type_int_id,
                    )],
                };

                if let Some((_, res_ty)) = operator_types
                    .iter()
                    .find(|(args, _)| args == &(lhs_type, rhs_type))
                {
                    let expr = Expression::InfixOp {
                        op: *op,
                        args: [lhs_id, rhs_id],
                    };
                    let ty = *res_ty;
                    (expr, ty)
                } else {
                    self.add_error(Error::TypeMismatchInfixOp {
                        op_fc: *op_fc,
                        op: *op,
                        lhs: lhs_id,
                        rhs: rhs_id,
                        expected_types: operator_types.into_iter().map(|(args, _)| args).collect(),
                    });
                    return None;
                }
            }
        };
        Some(self.prog.add_expression(fc, expr, ty))
    }
}
