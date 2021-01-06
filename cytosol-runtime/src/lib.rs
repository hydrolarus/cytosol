use std::collections::HashMap;

use rand::prelude::*;

use cytosol_hir as hir;
use hir::{
    types::{Bind, BindType, Enzyme, EnzymeId, ExpressionId, Gene, GeneId, Product, RecordId},
    Program,
};

pub mod value;

use crate::value::*;

type ExtFunc = Box<dyn for<'a> FnMut(&'a [Value])>;

#[derive(Default)]
pub struct ProgramContext {
    exts: HashMap<String, ExtFunc>,
}

impl ProgramContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_extern_function<Args, F>(&mut self, name: impl Into<String>, mut f: F)
    where
        for<'a> Args: FromValueSlice<'a>,
        for<'a> F: fn_ops::FnMut<Args, Output = ()> + 'static,
    {
        let f = move |args: &[Value]| f.call_mut(Args::from_value_slice(args));
        self.exts.insert(name.into(), Box::new(f));
    }

    pub fn run_gene(
        &mut self,
        prog: &Program,
        env: &mut CellEnv,
        vars: &mut RuntimeVars,
        gene_id: GeneId,
    ) {
        let gene = &prog[gene_id];

        for stmt_id in &gene.body {
            let stmt = &prog[*stmt_id];
            match stmt {
                hir::types::GeneStatement::Call { ext, arguments } => {
                    let ext = &prog[*ext];
                    let ext_fn = self.exts.get_mut(&ext.name.1).unwrap();

                    let fn_args = arguments
                        .iter()
                        .map(|id| eval_expr(prog, vars, *id).unwrap())
                        .collect::<Vec<_>>();

                    (ext_fn)(&fn_args[..]);
                }
                hir::types::GeneStatement::Express(prod) => {
                    eval_product(prog, env, vars, prod);
                }
            }
        }
    }
}

fn eval_product(prog: &Program, env: &mut CellEnv, vars: &RuntimeVars, prod: &Product) {
    match prod {
        Product::Enzyme {
            quantity: _,
            enzyme: _,
        } => todo!(),
        Product::Record {
            quantity,
            record,
            arguments,
        } => {
            let fields = arguments
                .iter()
                .map(|id| eval_expr(prog, vars, *id).unwrap())
                .collect::<Vec<_>>();

            env.add_record(*quantity, *record, fields);
        }
    }
}

fn eval_expr(prog: &Program, vars: &RuntimeVars, id: ExpressionId) -> Option<Value> {
    let expr = &prog[id];
    match expr {
        hir::types::Expression::IntegerLiteral(i) => Some(Value::Integer(*i as isize)),
        hir::types::Expression::StringLiteral(s) => Some(Value::String(s.clone())),
        hir::types::Expression::Variable(v) => vars.lookup(&v.1),
        hir::types::Expression::FieldAccess { base: _, field: _ } => todo!(),
        hir::types::Expression::PrefixOp { op: _, expr: _ } => todo!(),
        hir::types::Expression::InfixOp { op: _, args: _ } => todo!(),
    }
}

#[derive(Default)]
pub struct RuntimeVars {
    vals: HashMap<String, Value>,
}

impl RuntimeVars {
    pub fn clear(&mut self) {
        self.vals.clear();
    }

    pub fn lookup(&self, _name: &str) -> Option<Value> {
        todo!()
    }

    pub fn insert(&mut self, _name: String, _val: Value) {}
}

#[derive(Default)]
pub struct CellEnv {
    pub records: HashMap<RecordId, Vec<Value>>,
    pub enzymes: HashMap<EnzymeId, usize>,
}

impl CellEnv {
    pub fn summary(&self, sum: &mut CellEnvSummary) {
        sum.clear();
        for (id, v) in &self.records {
            sum.records.insert(*id, v.len());
        }

        for (id, n) in &self.enzymes {
            sum.enzymes.insert(*id, *n);
        }
    }

    pub fn add_record(&mut self, quantity: usize, record_id: RecordId, fields: Vec<Value>) {
        let recs = self.records.entry(record_id).or_default();

        recs.extend(std::iter::repeat(Value::Record(fields)).take(quantity));
    }
}

#[derive(Default, Debug)]
pub struct CellEnvSummary {
    pub records: HashMap<RecordId, usize>,
    pub enzymes: HashMap<EnzymeId, usize>,
}

impl CellEnvSummary {
    pub fn clear(&mut self) {
        self.records.clear();
        self.enzymes.clear();
    }

    pub fn check_bind(&self, bind: &Bind, bind_ty: &BindType) -> bool {
        let have_opt = match bind_ty {
            BindType::Record(id) => self.records.get(id),
            BindType::Enzyme(id) => self.enzymes.get(id),
        };

        let have = have_opt.copied().unwrap_or(0);

        match bind {
            Bind::None => have == 0,
            Bind::Quantity(need) => have >= *need,
            Bind::Named(_) => have >= 1,
        }
    }

    pub fn commit_bind(&mut self, bind: &Bind, bind_ty: &BindType) {
        let have_opt = match bind_ty {
            BindType::Record(id) => self.records.get_mut(id),
            BindType::Enzyme(id) => self.enzymes.get_mut(id),
        };

        let have = if let Some(n) = have_opt {
            n
        } else {
            return;
        };

        match bind {
            Bind::None => {
                debug_assert_eq!(*have, 0);
            }
            Bind::Quantity(need) => {
                debug_assert!(*have >= *need);
                *have -= *need;
            }
            Bind::Named(_) => {
                debug_assert!(*have >= 1);
                *have -= 1;
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct ExecutionContext {
    genes: Vec<GeneId>,
    enzymes: Vec<EnzymeId>,

    eligable_genes: Vec<GeneId>,
    eligable_enzymes: Vec<EnzymeId>,
}

impl ExecutionContext {
    fn clear(&mut self) {
        self.genes.clear();
        self.enzymes.clear();

        self.eligable_genes.clear();
        self.eligable_enzymes.clear();
    }

    fn shuffle(&mut self) {
        let mut rng = rand::thread_rng();
        self.genes.shuffle(&mut rng);
        self.enzymes.shuffle(&mut rng);
    }

    pub fn prepare_execution(&mut self, prog: &Program, summ: &mut CellEnvSummary) {
        self.clear();

        self.genes.extend(prog.genes.iter().map(|(id, _)| id));
        self.enzymes.extend(prog.enzymes.iter().map(|(id, _)| id));

        self.shuffle();

        self.eligable_genes.extend(
            self.genes
                .iter()
                .filter(|id| is_gene_eligable(&prog[**id], summ)),
        );
        self.eligable_enzymes.extend(
            self.enzymes
                .iter()
                .filter(|id| is_enzyme_eligable(&prog[**id], summ)),
        );
    }

    pub fn eligable_genes(&self) -> impl Iterator<Item = GeneId> + '_ {
        self.eligable_genes.iter().copied()
    }
}

fn is_gene_eligable(gene: &Gene, summ: &mut CellEnvSummary) -> bool {
    for (bind, ty) in &gene.binds {
        if !summ.check_bind(bind, ty) {
            return false;
        }
    }

    for (bind, ty) in &gene.binds {
        summ.commit_bind(bind, ty);
    }

    true
}

fn is_enzyme_eligable(enz: &Enzyme, summ: &mut CellEnvSummary) -> bool {
    for (bind, ty) in &enz.binds {
        if !summ.check_bind(bind, ty) {
            return false;
        }
    }

    for (bind, ty) in &enz.binds {
        summ.commit_bind(bind, ty);
    }

    true
}
