use criterion::{black_box, criterion_group, criterion_main, Criterion};
use cytosol::{
    driver::{DriverExecutionState, DriverRunner},
    hir::Program,
    runtime::{value::Value, CellEnv},
};

const SOURCE: &str = r#"
record Fib(a: int, b: int)

rule [f: Fib] -> Fib(a: f.b, b: f.a + f.b)
"#;

fn fibonacci_factory() -> impl FnMut(usize) -> usize {
    let mut prog = Program::new();
    let mut driver = DriverRunner::default();

    driver.add_file_from_string("fib.cyt", SOURCE.to_string());

    driver.compile(&mut prog).expect("Compilation failed");

    let fib_id = prog.record_by_name("Fib").unwrap();

    move |n: usize| {
        let mut env = CellEnv::default();
        let mut es = DriverExecutionState::default();

        env.add_record(1, fib_id, vec![Value::Integer(0), Value::Integer(1)]);

        driver.run(&prog, &mut es, &mut env, n);

        let x = &env.records[&fib_id][0];

        match &x[1] {
            Value::Integer(n) => *n as usize,
            _ => panic!(),
        }
    }
}

fn fibonacci(n: usize) -> usize {
    fibonacci_factory()(n)
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 50 (incl. compilation)", |b| {
        b.iter(|| fibonacci(black_box(50)))
    });

    let mut fib = fibonacci_factory();
    c.bench_function("fib 50 (precompiled)", |b| b.iter(|| fib(black_box(50))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
