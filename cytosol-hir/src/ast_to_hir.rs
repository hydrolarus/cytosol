use std::collections::HashMap;

use crate::{types::*, Program};

use ast::FC;
use cytosol_syntax as ast;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Atom {} was redefined", .redef_name.1)]
    RedefinedAtom {
        redef_name: ast::Identifier,
        orig_name: ast::Identifier,
    },
    #[error("Atom {} has duplicated field {}", .atom_name.1, .field_name.1)]
    DuplicateAtomField {
        atom_name: ast::Identifier,
        field_name: ast::Identifier,
        first_occurance: FC,
    },

    #[error("Recursive atom definitions")]
    RecursiveAtomDefinitions { defs: Vec<FC> },

    #[error("Type {} unknown", .name.1)]
    UnknownType { name: ast::Identifier },
}

pub fn files_to_hir(prog: &mut Program, files: &[ast::File]) -> Option<Vec<Error>> {
    let mut t = Translator {
        prog,
        errors: Default::default(),
    };

    let _ = t.translate_files(files);

    if t.errors.is_empty() {
        None
    } else {
        Some(t.errors)
    }
}

struct Translator<'a> {
    prog: &'a mut Program,
    errors: Vec<Error>,
}

impl Translator<'_> {
    fn add_error(&mut self, err: Error) {
        self.errors.push(err);
    }

    fn translate_files(&mut self, files: &[ast::File]) {
        // atoms first
        {
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
                        self.add_error(Error::RedefinedAtom {
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
                    name: atom.name.1.clone(),
                    field_names: vec![],
                    fields: vec![],
                };

                for (name, ty) in &atom.fields {
                    if hir_atom.field_names.contains(&name.1) {
                        self.add_error(Error::DuplicateAtomField {
                            atom_name: atom.name.clone(),
                            field_name: name.clone(),
                            first_occurance: atom
                                .fields
                                .iter()
                                .find(|(n, _)| n.1 == name.1)
                                .map(|(n, _)| n.0)
                                .unwrap(),
                        });
                    }

                    let ast::Type::Named(n) = ty;

                    if let Some(id) = self.prog.type_by_name(&n.1) {
                        hir_atom.fields.push(id);
                        hir_atom.field_names.push(name.1.clone());
                    } else {
                        self.add_error(Error::UnknownType { name: n.clone() });
                    }
                }

                let id = self.prog.add_atom(atom.fc, hir_atom).unwrap();
                self.prog.add_type(Type::Atom(id));
            }
        }

        // enzymes
        {}
    }
}
