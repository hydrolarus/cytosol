# `cytosol`

An embeddable programming language somewhat resembling cellular processes.

## State of the implementation

* [x] tokenising
* [x] parsing
* [x] semantic analysis and translation
* [x] runtime system and host API

## Overview of the language

> Records in the environment can activate gene functions or enzymes. Gene functions can express new record or enzymes into the environment. Enzymes can modify the records in the environment.

### `record`s

Records are like "`struct`s". They have a name and can have fields.

```
record PersonInfo(name: string, age: int)
```

In expressions, fields of records can be accessed with their name, such as `p.name` or `p.age`.

The name of a record can be used as a type.

```
record Student(final_grade: int, info: PersonInfo)
```

### Environment

The environment is a large unsorted set of `record`s and `enzyme`s. Records and enzymes can be added or removed from the environment.

To check or bind to a record or enzyme in the environment they need to be in the "execution factor" list of `gene` functions or in the "reactant list" of `enzyme`s.

A `cytosol` program can only add new things into the environment by using the `express` statement in `gene` functions.

The "host application" that manages the execution of `cytosol` program can also inject or remove `record`s or `enzyme`s freely.

### `gene` functions

`gene` functions are nameless functions with an "execution factor" list and a "body".

```
gene [2 A, 4 B, 0 C]
{
    express 10 D
}
```

The execution factor list is enclosed by `[` and `]`. An execution factor is an `record` or `enzyme` that is required to be in the "environment". Only when all execution factors are met/available will the body of the `gene` function run.

A number in front of the name of an `record` or `enzyme` signifies the quantity that needs to be available in the environment.
The quantity `0` means that the `record` or `enzyme` must *not* be present in order to run the body of the `gene` function.

An execution factor can be bound to a variable by using `name: TypeName`. This will bind **1** `record` or `enzyme` to the variable name.

The body can contain a list of a statement, which at the moment can only be
- the `express` statement to add new `record`s or `enzyme`s into the environment. For example `express 3 A` will place 3 new "`A`" `record`s or `enzyme`s into the environment.
- the `call` statement used to communicate with the host application

### `enzyme` functions

`enzyme` functions can modify the environment by transforming a set of `record`s or `enzyme`s into a new set of `record`s or `enzyme`s.

An `enzyme` function has a "reactant list", which states the part of the environment needing to be present for the `enzyme` to perform its function.

With all the reactants available the "product list" will be inserted into the environment and all the reactants will be removed.

```
enzyme MakeWater[4 Hydrogen, 2 Oxygen] -> 2 Water
```

Just like with the "execution factor list" of `gene` functions, reactants can have a quantity or be bound to a variable name.

Similarly to the `express` statement, the product list can also contain quantities.

### `extern` functions

With the `extern` keyword a function can be declared that can be called from within `gene` function with the `call` statement.

```
extern print_line(s: string)

// Similar to a "main" function.
//
// This expects the `Start` record to be inserted by the host
// application in order to run this gene function.
//
// The `StartInhibitor` is used to make this gene function
// run only once.
gene [Start, 0 StartInhibitor]
{
    call print_line(s: "hello world")
    express StartInhibitor
}

record Start
record StartInhibitor
```

Implementations for `extern` functions can be provided through the `ProgramContext` of the `DriverExecutionState`.

```rust
let mut exec_state = DriverExecutionState::default();

let ctx = exec_state.program_context();
ctx.set_extern_function("print_line", |s: String| println!("{}", s);
ctx.set_extern_function("print_string", |s: String| print!("{}", s);
ctx.set_extern_function("print_int", |i: isize| print!("{}", i));

```

## License

AGPLv3. Please see the [LICENSE](LICENSE) file.