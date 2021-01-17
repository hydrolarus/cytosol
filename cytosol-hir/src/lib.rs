use std::{collections::HashMap, ops::Index};

use cytosol_syntax::{Identifier, FC};
use id_arena::Arena;

pub mod ast_to_hir;
pub mod types;

use types::*;

pub use cytosol_syntax as ast;

pub struct Program {
    pub types: Arena<Type>,
    pub types_by_name: HashMap<String, TypeId>,
    pub exts: Arena<Extern>,
    pub exts_by_name: HashMap<String, ExternId>,
    pub exts_fc: HashMap<ExternId, FC>,
    pub records: Arena<Record>,
    pub records_by_name: HashMap<String, RecordId>,
    pub records_fc: HashMap<RecordId, FC>,

    pub rules: Arena<Rule>,
    pub rules_fc: HashMap<RuleId, FC>,
    pub genes: Arena<Gene>,
    pub genes_fc: HashMap<GeneId, FC>,
    pub gene_stmts: Arena<GeneStatement>,
    pub gene_stmts_fc: HashMap<GeneStatementId, FC>,

    pub exprs: Arena<Expression>,
    pub exprs_fc: HashMap<ExpressionId, FC>,
    pub exprs_type: HashMap<ExpressionId, TypeId>,

    pub type_int_id: TypeId,
    pub type_string_id: TypeId,
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
            records: Default::default(),
            records_by_name: Default::default(),
            records_fc: Default::default(),
            rules: Default::default(),
            rules_fc: Default::default(),
            genes: Default::default(),
            genes_fc: Default::default(),
            gene_stmts: Default::default(),
            gene_stmts_fc: Default::default(),
            exprs: Default::default(),
            exprs_fc: Default::default(),
            exprs_type: Default::default(),

            type_int_id,
            type_string_id,
        }
    }

    pub fn add_type(&mut self, val: Type) -> TypeId {
        match &val {
            Type::Record(record_id) => {
                let record: &Record = &self[*record_id];
                let name = record.name.1.clone();

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

    pub fn type_ident(&self, type_id: TypeId) -> Option<&Identifier> {
        let ty = self.types.get(type_id)?;
        match ty {
            Type::Int => None,
            Type::String => None,
            Type::Record(id) => self.record(*id).map(|a| &a.name),
        }
    }

    pub fn type_name(&self, type_id: TypeId) -> Option<(String, Option<FC>)> {
        let ty = self.types.get(type_id)?;

        match ty {
            Type::Int => Some(("int".to_string(), None)),
            Type::String => Some(("string".to_string(), None)),
            _ => {
                let ident = self.type_ident(type_id).unwrap();
                Some((ident.1.clone(), Some(ident.0)))
            }
        }
    }

    pub fn type_by_name(&self, name: &str) -> Option<TypeId> {
        self.types_by_name.get(name).copied()
    }

    pub fn add_extern(&mut self, fc: FC, val: Extern) -> Option<ExternId> {
        match self.exts_by_name.entry(val.name.1.clone()) {
            std::collections::hash_map::Entry::Occupied(_) => None,
            std::collections::hash_map::Entry::Vacant(entry) => {
                let id = self.exts.alloc(val);
                let _ = entry.insert(id);
                let overwritten = self.exts_fc.insert(id, fc).is_some();
                debug_assert_eq!(
                    overwritten, false,
                    "FC should only be inserted for a fresh ExternId"
                );
                Some(id)
            }
        }
    }

    pub fn extern_by_name(&self, name: &str) -> Option<ExternId> {
        self.exts_by_name.get(name).copied()
    }

    pub fn add_record(&mut self, fc: FC, val: Record) -> Option<RecordId> {
        let name = val.name.1.clone();

        if self.type_by_name(&name).is_some() {
            return None;
        }

        match self.records_by_name.entry(name) {
            std::collections::hash_map::Entry::Occupied(_) => {
                dbg!(fc, &val);
                None
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                let id = self.records.alloc(val);
                let _ = entry.insert(id);
                let overwritten = self.records_fc.insert(id, fc).is_some();
                debug_assert_eq!(
                    overwritten, false,
                    "FC should only be inserted for a fresh RecordId"
                );
                Some(id)
            }
        }
    }

    pub fn record_by_name(&self, name: &str) -> Option<RecordId> {
        self.records_by_name.get(name).copied()
    }

    pub fn add_rule(&mut self, fc: FC, rule: Rule) -> RuleId {
        let id = self.rules.alloc(rule);
        let overwritten = self.rules_fc.insert(id, fc).is_some();
        debug_assert_eq!(
            overwritten, false,
            "FC should only be inserted for a fresh RuleId"
        );
        id
    }

    pub fn add_gene(&mut self, fc: FC, val: Gene) -> GeneId {
        let id = self.genes.alloc(val);
        let overwritten = self.genes_fc.insert(id, fc).is_some();
        debug_assert_eq!(
            overwritten, false,
            "FC should only be inserted for a fresh GeneId"
        );
        id
    }

    pub fn add_gene_statement(&mut self, fc: FC, val: GeneStatement) -> GeneStatementId {
        let id = self.gene_stmts.alloc(val);
        self.gene_stmts_fc.insert(id, fc);
        id
    }

    pub fn add_expression(&mut self, fc: FC, val: Expression, ty: TypeId) -> ExpressionId {
        let id = self.exprs.alloc(val);
        let fc_overwritten = self.exprs_fc.insert(id, fc).is_some();
        debug_assert_eq!(
            fc_overwritten, false,
            "FC should only be inserted for a fresh ExpressionId"
        );
        let ty_overwritten = self.exprs_type.insert(id, ty).is_some();
        debug_assert_eq!(
            ty_overwritten, false,
            "TypeId should only be inserted for a fresh ExpressionId"
        );
        id
    }

    pub fn expr_type(&self, expr: ExpressionId) -> Option<TypeId> {
        self.exprs_type.get(&expr).copied()
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
    get_impl!(record, RecordId, Record, records);
    fc_impl!(record_fc, RecordId, records_fc);
    get_impl!(rule, RuleId, Rule, rules);
    fc_impl!(rule_fc, RuleId, rules_fc);
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
index_impl!(RecordId, Record, records);
index_impl!(RuleId, Rule, rules);
index_impl!(GeneId, Gene, genes);
index_impl!(GeneStatementId, GeneStatement, gene_stmts);
index_impl!(ExpressionId, Expression, exprs);
