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

    #[error("Atom {} has duplicated field {}", .atom_name.1, .field_name.1)]
    DuplicateAtomField {
        atom_name: ast::Identifier,
        field_name: ast::Identifier,
        first_occurance: FC,
    },

    #[error("Recursive atom definitions")]
    RecursiveAtomDefinitions { defs: Vec<FC> },

    #[error("Name {} rebound", .name.1)]
    NameRebound {
        item_fc: FC,
        name: Identifier,
        orig_name: Identifier,
    },

    #[error("Type {} unknown", .name.1)]
    UnknownType { name: Identifier },

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

    #[error("Enzymes ({}) do not have fields", .name.1)]
    ProductEnzymeWithField { fc: FC, name: Identifier },

    #[error("Product is missing the {} field", .missing_field.1)]
    ProductMissingAtomField {
        atom_name: Identifier,
        product_fc: FC,
        missing_field: Identifier,
    },

    #[error("")]
    ProductDuplicateAtomField {
        atom_name: Identifier,
        duplicate_field: Identifier,
        original_field: Identifier,
    },

    #[error("")]
    ProductUnknownAtomField {
        atom_name: Identifier,
        field: Identifier,
    },

    #[error("Undefined variable {}", .name.1)]
    UndefinedVariable {
        name: Identifier,
        in_scope: Vec<Identifier>,
    },

    #[error("Field index `.{}` on non atom type", .field_name.1)]
    FieldIndexOnNonAtom {
        field_name: Identifier,
        base_type: TypeId,
    },

    #[error("Invalid field index `.{}` on type {}", .field_name.1, .atom_name.1)]
    InvalidAtomFieldIndex {
        field_name: Identifier,
        atom_name: Identifier,
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
        // enzymes can be mutually recursive, so they all need to be added
        // without any binding information. The binding information then
        // gets added later on
        let mut enzymes = vec![];
        for file in files {
            for e in &file.enzymes {
                let enzyme = Enzyme {
                    name: e.name.clone(),
                    binds: vec![],
                    products: vec![],
                };
                if let Some(id) = self.prog.add_enzyme(e.fc, enzyme) {
                    self.prog.add_type(Type::Enzyme(id));
                    enzymes.push((id, e));
                } else {
                    let type_id = self.prog.type_by_name(&e.name.1).unwrap();
                    let prev_name = if let Some(n) = self.prog.type_ident(type_id) {
                        n.clone()
                    } else {
                        self.add_error(Error::RedefinedBuiltinType {
                            redef_name: e.name.clone(),
                        });
                        continue;
                    };
                    self.add_error(Error::RedefinedItem {
                        orig_name: prev_name,
                        redef_name: e.name.clone(),
                    });
                    continue;
                }
            }
        }

        self.setup_atoms(files);

        self.setup_enzymes(&enzymes);
    }

    fn setup_atoms(&mut self, files: &[ast::File]) {
        // sort by dependency
        // Because there are currently no optional types, so recursive
        // types are not allowed.
        let mut name_to_edge = HashMap::new();
        let mut name_to_fc = HashMap::new();

        type AtomIdx = (usize, usize);

        let mut g = petgraph::Graph::<AtomIdx, AtomIdx>::new();

        // generate all indices first
        for (f, file) in files.iter().enumerate() {
            for (a, atom) in file.atoms.iter().enumerate() {
                if let Some(prev) = name_to_fc.insert(&atom.name.1, &atom.name) {
                    self.add_error(Error::RedefinedItem {
                        orig_name: prev.clone(),
                        redef_name: atom.name.clone(),
                    });
                    continue;
                }

                let edge = g.add_node((f, a));
                name_to_edge.insert(atom.name.1.clone(), edge);
            }
        }

        // add all edges
        for file in files {
            for atom in &file.atoms {
                let self_id = &name_to_edge[&atom.name.1];

                let edges = atom
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
                // found a recursive set of atom definitions ⇒ error
                let defs: Vec<FC> = group
                    .into_iter()
                    .map(|i| g[i])
                    .map(|(f, a)| files[f].atoms[a].name.0)
                    .collect();
                self.add_error(Error::RecursiveAtomDefinitions { defs });
                continue;
            }

            debug_assert_eq!(group.len(), 1);

            let (f, a) = g[group[0]];
            let atom: &ast::Atom = &files[f].atoms[a];

            // check if atom depends on itself
            if atom
                .fields
                .iter()
                .any(|(_, ast::Type::Named(n))| n.1 == atom.name.1)
            {
                self.add_error(Error::RecursiveAtomDefinitions {
                    defs: vec![atom.name.0],
                });
                continue;
            }

            // atom is not self-recursive, nice!

            let mut hir_atom = Atom {
                name: atom.name.clone(),
                field_names: vec![],
                fields: vec![],
            };

            for (name, ty) in &atom.fields {
                if let Some(prev) = hir_atom.field_names.iter().find(|f| f.1 == name.1) {
                    self.add_error(Error::DuplicateAtomField {
                        atom_name: atom.name.clone(),
                        field_name: name.clone(),
                        first_occurance: prev.0,
                    });
                }

                let ast::Type::Named(n) = ty;

                if let Some(id) = self.prog.type_by_name(&n.1) {
                    hir_atom.fields.push(id);
                    hir_atom.field_names.push(name.clone());
                } else {
                    self.add_error(Error::UnknownType { name: n.clone() });
                }
            }

            if let Some(id) = self.prog.add_atom(atom.fc, hir_atom) {
                self.prog.add_type(Type::Atom(id));
            } else {
                let type_id = self.prog.type_by_name(&atom.name.1).unwrap();
                let prev_name = if let Some(n) = self.prog.type_ident(type_id) {
                    n.clone()
                } else {
                    self.add_error(Error::RedefinedBuiltinType {
                        redef_name: atom.name.clone(),
                    });
                    continue;
                };
                self.add_error(Error::RedefinedItem {
                    orig_name: prev_name,
                    redef_name: atom.name.clone(),
                });
            }
        }
    }

    // NOTE: atoms must be setup first! Otherwise they can't be used as products or
    // reactant types
    fn setup_enzymes(&mut self, enzymes: &[(EnzymeId, &ast::Enzyme)]) {
        // after they have all been added their binds are filled
        for (id, e) in enzymes {
            let mut binds = vec![];
            let mut bound_vars = VariableMap::new();

            for reactant in &e.reactants {
                let bind_attr = match &reactant.attr {
                    Some(ast::AtomBindingAttribute::Quantity(_, n)) => {
                        if *n == 0 {
                            Bind::None
                        } else {
                            Bind::Quantity(*n)
                        }
                    }
                    Some(ast::AtomBindingAttribute::Name(name)) => {
                        let ty = if let Some(ty) = self.prog.type_by_name(&reactant.name.1) {
                            ty
                        } else {
                            self.add_error(Error::UnknownType {
                                name: reactant.name.clone(),
                            });
                            continue;
                        };
                        if let Some((prev, _)) = bound_vars.insert(&name.1, (name.clone(), ty)) {
                            self.add_error(Error::NameRebound {
                                item_fc: e.fc,
                                name: name.clone(),
                                orig_name: prev,
                            });
                            continue;
                        }

                        Bind::Named(name.clone())
                    }
                    None => Bind::Quantity(1),
                };

                if let Some(atom_id) = self.prog.atom_by_name(&reactant.name.1) {
                    binds.push((bind_attr, BindType::Atom(atom_id)))
                } else if let Some(enz_id) = self.prog.enzyme_by_name(&reactant.name.1) {
                    binds.push((bind_attr, BindType::Enzyme(enz_id)));
                } else {
                    self.add_error(Error::UnknownType {
                        name: reactant.name.clone(),
                    });
                }
            }

            let products = e
                .products
                .iter()
                .flat_map(|p| self.translate_product(&bound_vars, p))
                .collect();

            let hir_e = &mut self.prog.enzymes[*id];
            hir_e.binds = binds;
            hir_e.products = products;
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
            Type::Atom(id) => {
                use std::collections::btree_map::Entry;

                let mut errs = vec![];

                let id = *id;

                let atom = self.prog[id].clone();

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
                            errs.push(Error::ProductDuplicateAtomField {
                                atom_name: atom.name.clone(),
                                duplicate_field: ident.clone(),
                                original_field: (*orig_ident).clone(),
                            });
                            continue;
                        }
                    }
                }

                for (field_name, field_ty) in atom.field_names.iter().zip(&atom.fields) {
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
                        errs.push(Error::ProductMissingAtomField {
                            atom_name: atom.name.clone(),
                            product_fc: product.fc,
                            missing_field: field_name.clone(),
                        });
                    }
                }

                for (_, (ident, _)) in call_fields {
                    errs.push(Error::ProductUnknownAtomField {
                        atom_name: atom.name.clone(),
                        field: ident.clone(),
                    });
                }

                self.errors.extend(errs);

                Some(Product::Atom {
                    quantity: product.quantity.map(|(_, n)| n).unwrap_or(1),
                    atom: id,
                    arguments: args,
                })
            }
            Type::Enzyme(id) => {
                // Enzymes don't have any fields
                if !product.fields.is_empty() {
                    self.add_error(Error::ProductEnzymeWithField {
                        name: product.name.clone(),
                        fc: product.fc,
                    });
                    return None;
                }
                Some(Product::Enzyme {
                    quantity: product.quantity.map(|(_, n)| n).unwrap_or(1),
                    enzyme: *id,
                })
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
                    Type::Atom(atom_id) => {
                        let atom = &self.prog[*atom_id];
                        if let Some((idx, _)) = atom
                            .field_names
                            .iter()
                            .enumerate()
                            .find(|(_, name)| name.1 == field_name.1)
                        {
                            let expr = Expression::FieldAccess {
                                base: base_id,
                                field: idx,
                            };
                            let ty = atom.fields[idx];
                            (expr, ty)
                        } else {
                            let err = Error::InvalidAtomFieldIndex {
                                atom_name: atom.name.clone(),
                                field_name: field_name.clone(),
                            };
                            self.add_error(err);
                            return None;
                        }
                    }
                    _ => {
                        self.errors.push(Error::FieldIndexOnNonAtom {
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
