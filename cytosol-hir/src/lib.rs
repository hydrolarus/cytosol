use std::{collections::HashMap, ops::Index};

use cytosol_syntax::FC;
use id_arena::Arena;

pub mod ast_to_hir;
pub mod types;

use types::*;

pub struct Program {
    pub(crate) types: Arena<Type>,
    pub(crate) types_by_name: HashMap<String, TypeId>,
    pub(crate) exts: Arena<Extern>,
    pub(crate) exts_by_name: HashMap<String, ExternId>,
    pub(crate) exts_fc: HashMap<ExternId, FC>,
    pub(crate) atoms: Arena<Atom>,
    pub(crate) atoms_by_name: HashMap<String, AtomId>,
    pub(crate) atoms_fc: HashMap<AtomId, FC>,

    pub(crate) enzymes: Arena<Enzyme>,
    pub(crate) enzymes_by_name: HashMap<String, EnzymeId>,
    pub(crate) enzymes_fc: HashMap<EnzymeId, FC>,
    pub(crate) genes: Arena<Gene>,
    pub(crate) genes_fc: HashMap<GeneId, FC>,
    pub(crate) gene_stmts: Arena<GeneStatement>,
    pub(crate) gene_stmts_fc: HashMap<GeneStatementId, FC>,

    pub(crate) exprs: Arena<Expression>,
    pub(crate) exprs_fc: HashMap<ExpressionId, FC>,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub fn new() -> Self {
        let mut types = Arena::new();
        let type_int_id = types.alloc(Type::Int);
        let type_string_id = types.alloc(Type::String);

        let mut types_by_name = HashMap::new();
        let _ = types_by_name.insert("int".to_string(), type_int_id);
        let _ = types_by_name.insert("string".to_string(), type_string_id);

        Self {
            types,
            types_by_name,
            exts: Default::default(),
            exts_by_name: Default::default(),
            exts_fc: Default::default(),
            atoms: Default::default(),
            atoms_by_name: Default::default(),
            atoms_fc: Default::default(),
            enzymes: Default::default(),
            enzymes_by_name: Default::default(),
            enzymes_fc: Default::default(),
            genes: Default::default(),
            genes_fc: Default::default(),
            gene_stmts: Default::default(),
            gene_stmts_fc: Default::default(),
            exprs: Default::default(),
            exprs_fc: Default::default(),
        }
    }

    pub fn add_type(&mut self, val: Type) -> TypeId {
        match &val {
            Type::Atom(atom_id) => {
                let atom = &self[*atom_id];
                let name = atom.name.clone();

                match self.types_by_name.entry(name) {
                    std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        let id = self.types.alloc(val);
                        let _ = entry.insert(id);
                        id
                    }
                }
            }
            Type::Int => unreachable!("Shouldn't ever add `int` as a type"),
            Type::String => unreachable!("Shouldn't ever add `string` as a type"),
        }
    }

    pub fn type_by_name(&self, name: &str) -> Option<TypeId> {
        self.types_by_name.get(name).copied()
    }

    pub fn add_extern(&mut self, fc: FC, val: Extern) -> Option<ExternId> {
        match self.exts_by_name.entry(val.name.clone()) {
            std::collections::hash_map::Entry::Occupied(_) => None,
            std::collections::hash_map::Entry::Vacant(entry) => {
                let id = self.exts.alloc(val);
                let _ = entry.insert(id);
                self.exts_fc.insert(id, fc).unwrap();
                Some(id)
            }
        }
    }

    pub fn extern_by_name(&self, name: &str) -> Option<ExternId> {
        self.exts_by_name.get(name).copied()
    }

    pub fn add_atom(&mut self, fc: FC, val: Atom) -> Option<AtomId> {
        let name = val.name.clone();

        match self.atoms_by_name.entry(name) {
            std::collections::hash_map::Entry::Occupied(_) => None,
            std::collections::hash_map::Entry::Vacant(entry) => {
                let id = self.atoms.alloc(val);
                let _ = entry.insert(id);
                self.atoms_fc.insert(id, fc).unwrap();
                Some(id)
            }
        }
    }

    pub fn atom_by_name(&self, name: &str) -> Option<AtomId> {
        self.atoms_by_name.get(name).copied()
    }

    pub fn add_enzyme(&mut self, fc: FC, val: Enzyme) -> Option<EnzymeId> {
        let name = val.name.clone();

        match self.enzymes_by_name.entry(name) {
            std::collections::hash_map::Entry::Occupied(_) => None,
            std::collections::hash_map::Entry::Vacant(entry) => {
                let id = self.enzymes.alloc(val);
                let _ = entry.insert(id);
                self.enzymes_fc.insert(id, fc).unwrap();
                Some(id)
            }
        }
    }

    pub fn enzyme_by_name(&self, name: &str) -> Option<EnzymeId> {
        self.enzymes_by_name.get(name).copied()
    }

    pub fn add_gene(&mut self, fc: FC, val: Gene) -> GeneId {
        let id = self.genes.alloc(val);
        self.genes_fc.insert(id, fc).unwrap();
        id
    }

    pub fn add_gene_statement(&mut self, fc: FC, val: GeneStatement) -> GeneStatementId {
        let id = self.gene_stmts.alloc(val);
        self.gene_stmts_fc.insert(id, fc);
        id
    }

    pub fn add_expression(&mut self, fc: FC, val: Expression) -> ExpressionId {
        let id = self.exprs.alloc(val);
        self.exprs_fc.insert(id, fc).unwrap();
        id
    }
}

macro_rules! get_impl {
    ($fn_name:ident, $tid:ty, $t:ty, $member:ident) => {
        pub fn $fn_name(&self, id: $tid) -> Option<&$t> {
            self.$member.get(id)
        }
    };
}

macro_rules! fc_impl {
    ($fn_name:ident, $tid:ty, $member:ident) => {
        pub fn $fn_name(&self, id: $tid) -> Option<FC> {
            self.$member.get(&id).copied()
        }
    };
}

impl Program {
    get_impl!(typ, TypeId, Type, types);

    get_impl!(ext, ExternId, Extern, exts);
    fc_impl!(ext_fc, ExternId, exts_fc);
    get_impl!(atom, AtomId, Atom, atoms);
    fc_impl!(atom_fc, AtomId, atoms_fc);
    get_impl!(enzyme, EnzymeId, Enzyme, enzymes);
    fc_impl!(enzyme_fc, EnzymeId, enzymes_fc);
    get_impl!(gene, GeneId, Gene, genes);
    fc_impl!(gene_fc, GeneId, genes_fc);
    get_impl!(gene_stmt, GeneStatementId, GeneStatement, gene_stmts);
    fc_impl!(gene_stmt_fc, GeneStatementId, gene_stmts_fc);
    get_impl!(expr, ExpressionId, Expression, exprs);
    fc_impl!(expr_fc, ExpressionId, exprs_fc);
}

macro_rules! index_impl {
    ($tid:ty, $t:ty, $member:ident) => {
        impl Index<$tid> for Program {
            type Output = $t;

            fn index(&self, index: $tid) -> &Self::Output {
                &self.$member[index]
            }
        }
    };
}

index_impl!(TypeId, Type, types);
index_impl!(ExternId, Extern, exts);
index_impl!(AtomId, Atom, atoms);
index_impl!(EnzymeId, Enzyme, enzymes);
index_impl!(GeneId, Gene, genes);
index_impl!(GeneStatementId, GeneStatement, gene_stmts);
index_impl!(ExpressionId, Expression, exprs);
