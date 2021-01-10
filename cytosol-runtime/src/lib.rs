use std::collections::HashMap;

use rand::prelude::*;

use cytosol_hir as hir;
use hir::{
    ast::{InfixOperator, PrefixOperator},
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
        vars.clear();

        let gene = &prog[gene_id];

        for (bind, bind_ty) in &gene.binds {
            env.apply_non_moving_bind(bind, bind_ty, vars);
        }

        for stmt_id in &gene.body {
            let stmt = &prog[*stmt_id];
            match stmt {
                hir::types::GeneStatement::Call { ext, arguments } => {
                    let ext = &prog[*ext];
                    let ext_fn = self
                        .exts
                        .get_mut(&ext.name.1)
                        .unwrap_or_else(|| panic!("Unbound extern function {}", ext.name.1));

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

    pub fn run_enzymes(
        &mut self,
        prog: &Program,
        env: &mut CellEnv,
        vars: &mut RuntimeVars,
        enzymes: impl Iterator<Item = (EnzymeId, usize)>,
    ) -> bool {
        let mut ran_any = false;
        for (id, n) in enzymes {
            let enzyme = &prog[id];
            run_enzyme(prog, env, vars, n, enzyme);
            ran_any = true;
        }
        ran_any
    }
}

fn run_enzyme(
    prog: &Program,
    env: &mut CellEnv,
    vars: &mut RuntimeVars,
    quantity: usize,
    enzyme: &Enzyme,
) {
    for _ in 0..quantity {
        vars.clear();
        for (bind, bind_ty) in &enzyme.binds {
            env.apply_moving_bind(bind, bind_ty, vars);
        }

        for prod in &enzyme.products {
            eval_product(prog, env, vars, prod);
        }
    }
}

fn eval_product(prog: &Program, env: &mut CellEnv, vars: &RuntimeVars, prod: &Product) {
    match prod {
        Product::Enzyme { quantity, enzyme } => {
            env.add_enzyme(*quantity, *enzyme);
        }
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
        hir::types::Expression::FieldAccess { base, field } => {
            let val = eval_expr(prog, vars, *base)?;
            if let Value::Record(mut fields) = val {
                Some(fields.remove(*field))
            } else {
                None
            }
        }
        hir::types::Expression::PrefixOp { op, expr } => {
            //
            let expr_val = eval_expr(prog, vars, *expr)?;
            match (op, expr_val) {
                (PrefixOperator::Neg, Value::Integer(i)) => Some(Value::Integer(-i)),
                _ => None,
            }
        }
        hir::types::Expression::InfixOp {
            op,
            args: [lhs, rhs],
        } => {
            let lhs_val = eval_expr(prog, vars, *lhs)?;
            let rhs_val = eval_expr(prog, vars, *rhs)?;

            match (op, lhs_val, rhs_val) {
                (InfixOperator::Add, Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a + b))
                }
                (InfixOperator::Add, Value::String(a), Value::String(b)) => {
                    Some(Value::String(a + &b))
                }
                (InfixOperator::Sub, Value::Integer(a), Value::Integer(b)) => {
                    Some(Value::Integer(a - b))
                }
                _ => None,
            }
        }
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

    pub fn lookup(&self, name: &str) -> Option<Value> {
        self.vals.get(name).cloned()
    }

    pub fn insert(&mut self, name: String, val: Value) {
        self.vals.insert(name, val);
    }
}

#[derive(Default, Debug)]
pub struct CellEnv {
    pub records: HashMap<RecordId, Vec<RecordFields>>,
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

    pub fn add_record(&mut self, quantity: usize, record_id: RecordId, fields: RecordFields) {
        let recs = self.records.entry(record_id).or_default();

        recs.extend(std::iter::repeat(fields).take(quantity));
    }

    pub fn add_enzyme(&mut self, quantity: usize, enzyme_id: EnzymeId) {
        let count = self.enzymes.entry(enzyme_id).or_default();
        *count += quantity;
    }

    pub fn apply_moving_bind(&mut self, bind: &Bind, bind_ty: &BindType, vars: &mut RuntimeVars) {
        let mut rng = rand::thread_rng();

        match bind_ty {
            BindType::Record(id) => {
                let recs = self.records.get_mut(id).unwrap();
                match bind {
                    Bind::None => {
                        debug_assert_eq!(recs.len(), 0);
                    }
                    Bind::Quantity(n) => {
                        debug_assert!(recs.len() >= *n);
                        for _ in 0..*n {
                            let idx = rng.gen_range(0..recs.len());
                            let _ = recs.swap_remove(idx);
                        }
                    }
                    Bind::Named(name) => {
                        let idx = rng.gen_range(0..recs.len());

                        let fields = recs.swap_remove(idx);
                        vars.insert(name.1.clone(), Value::Record(fields));
                    }
                }
            }
            BindType::Enzyme(id) => {
                let enzys = self.enzymes.get_mut(id).unwrap();
                match bind {
                    Bind::None => {
                        debug_assert_eq!(*enzys, 0);
                    }
                    Bind::Quantity(n) => {
                        debug_assert!(*enzys >= *n);
                        *enzys -= *n;
                    }
                    Bind::Named(name) => {
                        debug_assert!(*enzys >= 1);
                        *enzys -= 1;
                        vars.insert(name.1.clone(), Value::Enzyme);
                    }
                }
            }
        }
    }

    pub fn apply_non_moving_bind(
        &mut self,
        bind: &Bind,
        bind_ty: &BindType,
        vars: &mut RuntimeVars,
    ) {
        let mut rng = rand::thread_rng();

        match bind_ty {
            BindType::Record(id) => {
                let empty_vec = vec![];
                let recs = self.records.get(id).unwrap_or(&empty_vec);
                match bind {
                    Bind::None => {
                        debug_assert_eq!(recs.len(), 0);
                    }
                    Bind::Quantity(n) => {
                        debug_assert!(recs.len() >= *n);
                    }
                    Bind::Named(name) => {
                        let idx = rng.gen_range(0..recs.len());

                        let fields = recs[idx].clone();
                        vars.insert(name.1.clone(), Value::Record(fields));
                    }
                }
            }
            BindType::Enzyme(id) => {
                let enzys = self.enzymes.get(id).unwrap_or(&0);
                match bind {
                    Bind::None => {
                        debug_assert_eq!(*enzys, 0);
                    }
                    Bind::Quantity(n) => {
                        debug_assert!(*enzys >= *n);
                    }
                    Bind::Named(name) => {
                        debug_assert!(*enzys >= 1);
                        vars.insert(name.1.clone(), Value::Enzyme);
                    }
                }
            }
        }
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
pub struct ExecutionPlan {
    genes: Vec<GeneId>,
    enzymes: Vec<EnzymeId>,

    eligable_genes: Vec<GeneId>,

    eligable_enzyme_ids: Vec<EnzymeId>,
    eligable_enzymes: HashMap<EnzymeId, usize>,
}

impl ExecutionPlan {
    fn clear(&mut self) {
        self.genes.clear();
        self.enzymes.clear();

        self.eligable_genes.clear();
        self.eligable_enzyme_ids.clear();
        self.eligable_enzymes.clear();
    }

    fn shuffle(&mut self) {
        let mut rng = rand::thread_rng();
        self.genes.shuffle(&mut rng);
        self.enzymes.shuffle(&mut rng);
    }

    pub fn prepare_gene_execution(&mut self, prog: &Program, summ: &mut CellEnvSummary) {
        self.clear();

        self.genes.extend(prog.genes.iter().map(|(id, _)| id));
        self.enzymes.extend(
            prog.enzymes
                .iter()
                .map(|(id, _)| id)
                .filter(|id| summ.enzymes.get(id).copied().unwrap_or(0) > 0),
        );

        self.shuffle();

        self.eligable_genes.extend(
            self.genes
                .iter()
                .filter(|id| is_gene_eligable(&prog[**id], summ)),
        );
    }

    pub fn prepare_enzyme_execution(&mut self, prog: &Program, summ: &mut CellEnvSummary) {
        self.clear();

        self.genes.extend(prog.genes.iter().map(|(id, _)| id));
        self.enzymes.extend(
            prog.enzymes
                .iter()
                .map(|(id, _)| id)
                .filter(|id| summ.enzymes.get(id).copied().unwrap_or(0) > 0),
        );

        self.shuffle();

        self.eligable_enzyme_ids.extend(
            self.enzymes
                .iter()
                .filter(|id| is_enzyme_eligable(&prog[**id], summ)),
        );

        // all enzymes can "run" at least once.
        self.eligable_enzymes
            .extend(self.eligable_enzyme_ids.iter().map(|x| (*x, 1)));

        {
            let mut rng = rand::thread_rng();
            let mut still_eligable = self.eligable_enzyme_ids.clone();
            let mut to_remove = vec![];

            loop {
                if still_eligable.is_empty() {
                    break;
                }

                still_eligable.shuffle(&mut rng);

                for (idx, id) in still_eligable.iter().enumerate() {
                    if is_enzyme_eligable(&prog[*id], summ) {
                        *self.eligable_enzymes.get_mut(id).unwrap() += 1;
                    } else {
                        to_remove.push(idx);
                    }
                }

                to_remove.sort_unstable_by(|a, b| b.cmp(a));

                for idx in to_remove.drain(..) {
                    still_eligable.swap_remove(idx);
                }
            }
        }
    }

    pub fn eligable_genes(&self) -> impl Iterator<Item = GeneId> + '_ {
        self.eligable_genes.iter().copied()
    }

    pub fn eligable_enzymes(&self) -> impl Iterator<Item = (EnzymeId, usize)> + '_ {
        self.eligable_enzymes
            .iter()
            .map(|(i, n)| (*i, *n))
            .filter(|(_, n)| *n > 0)
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
