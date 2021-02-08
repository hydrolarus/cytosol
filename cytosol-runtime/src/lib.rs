use std::collections::HashMap;

use rand::prelude::*;

use cytosol_hir as hir;
use hir::{
    ast::{InfixOperator, PrefixOperator},
    types::{Bind, ExpressionId, Gene, GeneId, Product, RecordId, Rule, RuleId},
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

    pub fn set_extern_function_raw(
        &mut self,
        name: impl Into<String>,
        mut f: impl FnMut(&[Value]) + 'static,
    ) {
        let f = move |args: &[Value]| f(args);
        self.exts.insert(name.into(), Box::new(f));
    }

    pub fn set_extern_function<Args, F>(&mut self, name: impl Into<String>, mut f: F)
    where
        for<'a> Args: FromValueSlice<'a>,
        for<'a> F: fn_ops::FnMut<Args, Output = ()> + 'static,
    {
        self.set_extern_function_raw(name, move |args: &[Value]| {
            f.call_mut(Args::from_value_slice(args))
        })
    }
}

pub fn run_gene(
    ctx: &mut ProgramContext,
    prog: &Program,
    env: &mut CellEnv,
    vars: &mut RuntimeVars,
    gene_id: GeneId,
) {
    vars.clear();

    let gene = &prog[gene_id];

    for (bind, rec) in &gene.binds {
        env.apply_non_moving_bind(bind, *rec, vars);
    }

    for stmt_id in &gene.body {
        let stmt = &prog[*stmt_id];
        match stmt {
            hir::types::GeneStatement::Call { ext, arguments } => {
                let ext = &prog[*ext];
                let ext_fn = ctx
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

pub fn run_rules(
    prog: &Program,
    env: &mut CellEnv,
    vars: &mut RuntimeVars,
    rules: impl Iterator<Item = (RuleId, usize)>,
) -> bool {
    let mut ran_any = false;
    for (id, n) in rules {
        let rule = &prog[id];
        run_rule(prog, env, vars, n, rule);
        ran_any = true;
    }
    ran_any
}

fn run_rule(
    prog: &Program,
    env: &mut CellEnv,
    vars: &mut RuntimeVars,
    quantity: usize,
    rule: &Rule,
) {
    for _ in 0..quantity {
        vars.clear();
        for (bind, rec) in &rule.binds {
            env.apply_moving_bind(bind, *rec, vars);
        }

        for prod in &rule.products {
            eval_product(prog, env, vars, prod);
        }
    }
}

fn eval_product(prog: &Program, env: &mut CellEnv, vars: &RuntimeVars, prod: &Product) {
    let fields = prod
        .arguments
        .iter()
        .map(|id| eval_expr(prog, vars, *id).unwrap())
        .collect::<Vec<_>>();

    env.add_record(prod.quantity, prod.record, fields);
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
}

impl CellEnv {
    pub fn summary(&self, sum: &mut CellEnvSummary) {
        sum.clear();
        for (id, v) in &self.records {
            sum.records.insert(*id, v.len());
        }
    }

    pub fn add_record(&mut self, quantity: usize, record_id: RecordId, fields: RecordFields) {
        let recs = self.records.entry(record_id).or_default();

        recs.extend(std::iter::repeat(fields).take(quantity));
    }

    pub fn count_records(&self, record_id: RecordId) -> usize {
        self.records
            .get(&record_id)
            .map(|vals| vals.len())
            .unwrap_or(0)
    }

    pub fn apply_moving_bind(&mut self, bind: &Bind, record: RecordId, vars: &mut RuntimeVars) {
        let mut rng = rand::thread_rng();

        let recs = self.records.get_mut(&record).unwrap();
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

    pub fn apply_non_moving_bind(
        &mut self,
        bind: &Bind,
        record_id: RecordId,
        vars: &mut RuntimeVars,
    ) {
        let mut rng = rand::thread_rng();

        let empty_vec = vec![];
        let recs = self.records.get(&record_id).unwrap_or(&empty_vec);
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
}

#[derive(Default, Debug)]
pub struct CellEnvSummary {
    pub records: HashMap<RecordId, usize>,
}

impl CellEnvSummary {
    pub fn clear(&mut self) {
        self.records.clear();
    }

    pub fn check_bind(&self, bind: &Bind, record_id: RecordId) -> bool {
        let have = self.records.get(&record_id).copied().unwrap_or(0);

        match bind {
            Bind::None => have == 0,
            Bind::Quantity(need) => have >= *need,
            Bind::Named(_) => have >= 1,
        }
    }

    pub fn commit_bind(&mut self, bind: &Bind, record_id: RecordId) {
        let have_opt = self.records.get_mut(&record_id);

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
    rules: Vec<RuleId>,

    eligable_genes: Vec<GeneId>,

    eligable_rule_ids: Vec<RuleId>,
    eligable_rules: HashMap<RuleId, usize>,
}

impl ExecutionPlan {
    fn clear(&mut self) {
        self.genes.clear();
        self.rules.clear();

        self.eligable_genes.clear();
        self.eligable_rule_ids.clear();
        self.eligable_rules.clear();
    }

    pub fn prepare_gene_execution(&mut self, prog: &Program, summ: &mut CellEnvSummary) {
        self.clear();

        self.genes.extend(prog.genes.iter().map(|(id, _)| id));
        self.genes.shuffle(&mut rand::thread_rng());

        self.eligable_genes.extend(
            self.genes
                .iter()
                .filter(|id| is_gene_eligable(&prog[**id], summ)),
        );
    }

    pub fn prepare_rule_execution(&mut self, prog: &Program, summ: &mut CellEnvSummary) {
        self.clear();

        self.rules.extend(prog.rules.iter().map(|(id, _)| id));
        self.rules.shuffle(&mut rand::thread_rng());

        self.eligable_rule_ids.extend(
            self.rules
                .iter()
                .filter(|id| is_rule_eligable(&prog[**id], summ)),
        );

        // all rules can "run" at least once.
        self.eligable_rules
            .extend(self.eligable_rule_ids.iter().map(|x| (*x, 1)));

        {
            let mut rng = rand::thread_rng();
            let mut still_eligable = self.eligable_rule_ids.clone();
            let mut to_remove = vec![];

            loop {
                if still_eligable.is_empty() {
                    break;
                }

                still_eligable.shuffle(&mut rng);

                for (idx, id) in still_eligable.iter().enumerate() {
                    if is_rule_eligable(&prog[*id], summ) {
                        *self.eligable_rules.get_mut(id).unwrap() += 1;
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

    pub fn eligable_rules(&self) -> impl Iterator<Item = (RuleId, usize)> + '_ {
        self.eligable_rules
            .iter()
            .map(|(i, n)| (*i, *n))
            .filter(|(_, n)| *n > 0)
    }
}

fn is_gene_eligable(gene: &Gene, summ: &mut CellEnvSummary) -> bool {
    for (bind, rec) in &gene.binds {
        if !summ.check_bind(bind, *rec) {
            return false;
        }
    }

    for (bind, rec) in &gene.binds {
        summ.commit_bind(bind, *rec);
    }

    true
}

fn is_rule_eligable(rule: &Rule, summ: &mut CellEnvSummary) -> bool {
    for (bind, rec) in &rule.binds {
        if !summ.check_bind(bind, *rec) {
            return false;
        }
    }

    for (bind, rec) in &rule.binds {
        summ.commit_bind(bind, *rec);
    }

    true
}
