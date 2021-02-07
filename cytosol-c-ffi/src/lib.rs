use std::ffi::CStr;

use id_arena::ArenaBehavior;

//
// Program
//

pub struct Program(cytosol::hir::Program);

#[no_mangle]
pub extern "C" fn cyt_program_new() -> Box<Program> {
    Box::new(Program(cytosol::hir::Program::new()))
}

#[no_mangle]
pub extern "C" fn cyt_program_destroy(prog: Box<Program>) {
    drop(prog);
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct RecordId(u32, usize);

impl RecordId {
    fn from_id(id: cytosol::hir::types::RecordId) -> Self {
        let arena_id = id_arena::DefaultArenaBehavior::arena_id(id);
        let idx = id_arena::DefaultArenaBehavior::index(id);
        Self(arena_id, idx)
    }

    fn to_id(self) -> cytosol::hir::types::RecordId {
        id_arena::DefaultArenaBehavior::new_id(self.0, self.1)
    }
}

/// # Safety
/// `name` must be a valid pointer to a UTF-8 and NUL-terminated string.
#[no_mangle]
pub unsafe extern "C" fn cyt_program_record_by_name(
    prog: &Program,
    name: *const std::os::raw::c_char,
    out_id: &mut RecordId,
) -> bool {
    let name = CStr::from_ptr(name);
    if let Some(id) = prog.0.record_by_name(&name.to_string_lossy()) {
        *out_id = RecordId::from_id(id);
        true
    } else {
        false
    }
}

//
// Driver runner
//

pub struct DriverRunner(cytosol::driver::DriverRunner);

#[no_mangle]
pub extern "C" fn cyt_driver_runner_new() -> Box<DriverRunner> {
    Box::new(DriverRunner(cytosol::driver::DriverRunner::default()))
}

#[no_mangle]
pub extern "C" fn cyt_driver_runner_destroy(driver_runner: Box<DriverRunner>) {
    drop(driver_runner);
}

/// # Safety
/// `name` and `source` must be pointers to valid UTF-8 and NUL-terminated strings.
#[no_mangle]
pub unsafe extern "C" fn cyt_driver_runner_add_file_from_string(
    r: &mut DriverRunner,
    name: *const std::os::raw::c_char,
    source: *const std::os::raw::c_char,
) {
    let name_cstr = CStr::from_ptr(name);
    let source_cstr = CStr::from_ptr(source);
    r.0.add_file_from_string(
        name_cstr.to_string_lossy(),
        source_cstr.to_string_lossy().into_owned(),
    );
}

/// If an error occurs `false` is returned.
/// In that case the error will also be directly written to stdout.
/// If no error occurs then `true` is returned.
#[no_mangle]
pub extern "C" fn cyt_driver_runner_compile(r: &mut DriverRunner, prog: &mut Program) -> bool {
    match r.0.compile(&mut prog.0) {
        Ok(()) => true,
        Err(err) => {
            r.0.report_error(&prog.0, &err, true);
            false
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunResult {
    MadeProgress,
    NoProgress,
}

impl RunResult {
    fn from_rust_result(res: cytosol::driver::RunResult) -> Self {
        match res {
            cytosol::driver::RunResult::MadeProgress => Self::MadeProgress,
            cytosol::driver::RunResult::NoProgress => Self::NoProgress,
        }
    }
}

/// Run the program for one single iteration.
#[no_mangle]
pub extern "C" fn cyt_driver_runner_run_single_iteration(
    r: &mut DriverRunner,
    prog: &Program,
    exec_state: &mut ExecutionState,
    cell_env: &mut CellEnv,
) -> RunResult {
    let res =
        r.0.run_single_iteration(&prog.0, &mut exec_state.0, &mut cell_env.0);
    RunResult::from_rust_result(res)
}

/// Run the program for multiple iterations.
///
/// This runs the program until it either no longer makes progress or
/// the iteration bound was reached.
///
/// An iteration bound of `0` means that there is no bound.
#[no_mangle]
pub extern "C" fn cyt_driver_runner_run(
    r: &mut DriverRunner,
    prog: &Program,
    exec_state: &mut ExecutionState,
    cell_env: &mut CellEnv,
    iter_bound: usize,
) {
    let bound = if iter_bound == 0 {
        None
    } else {
        Some(iter_bound)
    };

    r.0.run(&prog.0, &mut exec_state.0, &mut cell_env.0, bound);
}

//
// Values
//

#[repr(C)]
pub enum ValueType {
    Integer,
    String,
    Record,
}

pub struct Value(cytosol::runtime::value::Value);

#[no_mangle]
pub extern "C" fn cyt_value_new_integer(n: isize) -> Box<Value> {
    Box::new(Value(cytosol::runtime::value::Value::Integer(n)))
}

/// # Safety
/// `s` must be a valid pointer to a UTF-8 and NUL-terminated string.
#[no_mangle]
pub unsafe extern "C" fn cyt_value_new_string(s: *const std::os::raw::c_char) -> Box<Value> {
    let cstr = CStr::from_ptr(s);
    Box::new(Value(cytosol::runtime::value::Value::String(
        cstr.to_string_lossy().into_owned(),
    )))
}

#[no_mangle]
pub extern "C" fn cyt_value_new_record() -> Box<Value> {
    Box::new(Value(cytosol::runtime::value::Value::Record(vec![])))
}

/// Adds the `new_field` value to the record in `record`.
///
/// If `record` is not a value created with `cyt_value_new_record` then this function has no effect.
///
/// The `new_field` value will transfer ownership, so the `destroy` function *must not* be called
/// on that value again.
#[no_mangle]
pub extern "C" fn cyt_value_record_add_field(record: &mut Value, new_field: Box<Value>) {
    match &mut record.0 {
        cytosol::runtime::value::Value::Integer(_) => {}
        cytosol::runtime::value::Value::String(_) => {}
        cytosol::runtime::value::Value::Record(fields) => {
            fields.push(new_field.0);
        }
    }
}

#[no_mangle]
pub extern "C" fn cyt_value_destroy(value: Box<Value>) {
    drop(value);
}

/// Get the type of the `value`.
#[no_mangle]
pub extern "C" fn cyt_value_get_type(value: &Value) -> ValueType {
    match &value.0 {
        cytosol::runtime::value::Value::Integer(_) => ValueType::Integer,
        cytosol::runtime::value::Value::String(_) => ValueType::String,
        cytosol::runtime::value::Value::Record(_) => ValueType::Record,
    }
}

/// Get the integer value in `value` by writing it in `out_i`.
///
/// If `value` is not an integer then `false` is returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_get_integer(value: &Value, out_i: &mut isize) -> bool {
    match &value.0 {
        cytosol::runtime::value::Value::Integer(v) => {
            *out_i = *v;
            true
        }
        cytosol::runtime::value::Value::String(_) => false,
        cytosol::runtime::value::Value::Record(_) => false,
    }
}

/// Get the string value in `value` by writing a pointer to `out_ptr` and the length to `out_len`.
///
/// The string is **NOT** NUL-terminated.
///
/// If `value` is not a string then `false` is returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_get_string(
    value: &Value,
    out_ptr: &mut *const std::os::raw::c_char,
    out_len: &mut usize,
) -> bool {
    match &value.0 {
        cytosol::runtime::value::Value::Integer(_) => false,
        cytosol::runtime::value::Value::String(s) => {
            *out_ptr = s.as_ptr() as *const std::os::raw::c_char;
            *out_len = s.len();
            true
        }
        cytosol::runtime::value::Value::Record(_) => false,
    }
}

/// Get a field value of the record in `value` at index `index` by creating a
/// copy of the field and writing it to `out_value`.
///
/// The value in `out_value` will be owned, so the `destroy` function needs to
/// be called.
///
/// If `value` is not a record or if `index` is out of bounds then `false` is
/// returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_get_record_field(
    value: &Value,
    index: usize,
    out_value: &mut *const Value,
) -> bool {
    match &value.0 {
        cytosol::runtime::value::Value::Integer(_) => false,
        cytosol::runtime::value::Value::String(_) => false,
        cytosol::runtime::value::Value::Record(r) => {
            if let Some(s) = r.get(index) {
                *out_value = Box::into_raw(Box::new(Value(s.clone())));
                true
            } else {
                false
            }
        }
    }
}

//
// Cell environment
//

pub struct CellEnv(cytosol::runtime::CellEnv);

#[no_mangle]
pub extern "C" fn cyt_cellenv_new() -> Box<CellEnv> {
    Box::new(CellEnv(cytosol::runtime::CellEnv::default()))
}

#[no_mangle]
pub extern "C" fn cyt_cellenv_destroy(cell_env: Box<CellEnv>) {
    drop(cell_env);
}

/// Add a record with id `record_id` to the environment `quantity` times.
///
/// The `fields` will be copied
///
/// # Safety
/// `fields` must be a valid pointer to an array of values allocated by the
/// `cyt_value_` functions with `num_fields` elements.
#[no_mangle]
pub unsafe extern "C" fn cyt_cellenv_add_record(
    cell_env: &mut CellEnv,
    quantity: usize,
    record_id: RecordId,
    num_fields: usize,
    fields: *const *const Value,
) {
    let fields_slice = std::slice::from_raw_parts(fields, num_fields);
    let fields = fields_slice.iter().map(|v| (**v).0.clone()).collect();
    cell_env.0.add_record(quantity, record_id.to_id(), fields)
}

//
// Execution state
//

pub struct ExecutionState(cytosol::driver::DriverExecutionState);

#[no_mangle]
pub extern "C" fn cyt_exec_state_new() -> Box<ExecutionState> {
    Box::new(ExecutionState(
        cytosol::driver::DriverExecutionState::default(),
    ))
}

#[no_mangle]
pub extern "C" fn cyt_exec_state_destroy(exec_state: Box<ExecutionState>) {
    drop(exec_state)
}

/// # Safety
/// `s` must be a valid pointer to a UTF-8 and NUL-terminated string.
/// `f` must be a valid function pointer.
#[no_mangle]
pub unsafe extern "C" fn cyt_exec_state_set_extern_function(
    exec_state: &mut ExecutionState,
    name: *const std::os::raw::c_char,
    f: extern "C" fn(*mut std::os::raw::c_void, usize, *const *const Value),
    data: *mut std::os::raw::c_void,
) {
    let name = CStr::from_ptr(name);

    let ctx = exec_state.0.program_context();
    ctx.set_extern_function_raw(name.to_string_lossy().into_owned(), move |args| {
        let boxed = args
            .iter()
            .map(|s| Box::new(Value(s.clone())))
            .collect::<Vec<_>>();
        f(data, boxed.len(), boxed.as_ptr() as *const *const Value);
    });
}
