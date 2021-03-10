use std::ffi::CStr;

use cytosol::runtime::value::Value;
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

/// Load a file from a path
///
/// When the loading was successful, `0` is returned.
/// If an error occured then the OS error code will be returned.
/// If there was an error but no OS error code is present then `-1` will be returned.
///
/// # Safety
/// `path` must be a pointer to a NUL-terminated string representing a path.
#[no_mangle]
pub unsafe extern "C" fn cyt_driver_runner_add_file_from_path(
    r: &mut DriverRunner,
    path: *const std::os::raw::c_char,
) -> i32 {
    let name_cstr = CStr::from_ptr(path);
    match r
        .0
        .add_file_from_path(name_cstr.to_string_lossy().into_owned())
    {
        Ok(_) => 0,
        Err(err) => err.raw_os_error().unwrap_or(-1),
    }
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
    Bool,
    Integer,
    String,
    Record,
}

pub struct ValueBuffer(Vec<Value>);

#[no_mangle]
pub extern "C" fn cyt_value_buffer_new(size: usize) -> Box<ValueBuffer> {
    Box::new(ValueBuffer(vec![Value::Integer(0); size]))
}

#[no_mangle]
pub extern "C" fn cyt_value_buffer_get_size(buf: &ValueBuffer) -> usize {
    buf.0.len()
}

#[no_mangle]
pub extern "C" fn cyt_value_buffer_set_bool(buf: &mut ValueBuffer, idx: usize, b: bool) {
    if let Some(val) = buf.0.get_mut(idx) {
        *val = Value::Bool(b);
    }
}

#[no_mangle]
pub extern "C" fn cyt_value_buffer_set_int(buf: &mut ValueBuffer, idx: usize, i: isize) {
    if let Some(val) = buf.0.get_mut(idx) {
        *val = Value::Integer(i);
    }
}

/// # Safety
/// `s` must be a valid pointer to a UTF-8 and NUL-terminated string.
#[no_mangle]
pub unsafe extern "C" fn cyt_value_buffer_set_string(
    buf: &mut ValueBuffer,
    idx: usize,
    s: *const std::os::raw::c_char,
) {
    let cstr = CStr::from_ptr(s);
    if let Some(val) = buf.0.get_mut(idx) {
        *val = Value::String(cstr.to_string_lossy().into_owned());
    }
}

/// # Safety
/// `fields` will be consumed, do **not** call the destructor on the value buffer
#[no_mangle]
pub extern "C" fn cyt_value_buffer_set_record(
    buf: &mut ValueBuffer,
    idx: usize,
    fields: Box<ValueBuffer>,
) {
    if let Some(val) = buf.0.get_mut(idx) {
        *val = Value::Record(fields.0);
    }
}

#[no_mangle]
pub extern "C" fn cyt_value_buffer_destroy(buf: Box<ValueBuffer>) {
    drop(buf);
}

/// Get the type of the value at index `idx`.
///
/// If the index is out of bounds then `Integer` will be returned.
#[no_mangle]
pub extern "C" fn cyt_value_get_type(buf: &ValueBuffer, idx: usize) -> ValueType {
    match buf.0.get(idx) {
        Some(Value::Bool(_)) => ValueType::Bool,
        Some(Value::Integer(_)) => ValueType::Integer,
        Some(Value::String(_)) => ValueType::String,
        Some(Value::Record(_)) => ValueType::Record,
        None => ValueType::Integer,
    }
}

/// Get the boolean value in `buf` at `idx` by writing it in `out_b`.
///
/// If the value is not a boolean then `false` is returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_buffer_get_bool(
    buf: &ValueBuffer,
    idx: usize,
    out_b: &mut bool,
) -> bool {
    match buf.0.get(idx) {
        Some(Value::Bool(b)) => {
            *out_b = *b;
            true
        }
        _ => false,
    }
}

/// Get the integer value in `buf` at `idx` by writing it in `out_i`.
///
/// If the value is not an integer then `false` is returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_buffer_get_int(
    buf: &ValueBuffer,
    idx: usize,
    out_i: &mut isize,
) -> bool {
    match buf.0.get(idx) {
        Some(Value::Integer(v)) => {
            *out_i = *v;
            true
        }
        _ => false,
    }
}

/// Get the string value in `buf` at `idx` by writing a pointer to `out_ptr`
/// and the length to `out_len`.
///
/// The string is **NOT** NUL-terminated.
///
/// If the value is not a string then `false` is returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_buffer_get_string(
    buf: &ValueBuffer,
    idx: usize,
    out_ptr: &mut *const std::os::raw::c_char,
    out_len: &mut usize,
) -> bool {
    match buf.0.get(idx) {
        Some(Value::String(s)) => {
            *out_ptr = s.as_ptr() as *const std::os::raw::c_char;
            *out_len = s.len();
            true
        }
        _ => false,
    }
}

/// Get the field value buffer of the record in `buf` at `idx`.
///
/// The value buffer in `out_value` will be owned, so the `destroy` function
/// needs to be called.
///
/// If the value at `idx` is not a record or if `idx` is out of bounds then
/// `false` is returned, `true` otherwise.
#[no_mangle]
pub extern "C" fn cyt_value_buffer_get_record_fields(
    buf: &ValueBuffer,
    idx: usize,
    out_value: &mut *mut ValueBuffer,
) -> bool {
    match buf.0.get(idx) {
        Some(Value::Record(r)) => {
            *out_value = Box::into_raw(Box::new(ValueBuffer(r.clone())));
            true
        }
        _ => false,
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
/// The ownership of `fields` will be transferred, so **do not** call the
/// destroy function on this value buffer.
#[no_mangle]
pub extern "C" fn cyt_cellenv_add_record(
    cell_env: &mut CellEnv,
    quantity: usize,
    record_id: RecordId,
    fields: Box<ValueBuffer>,
) {
    cell_env.0.add_record(quantity, record_id.to_id(), fields.0);
}

#[no_mangle]
pub extern "C" fn cyt_cellenv_count_records(cell_env: &CellEnv, record_id: RecordId) -> usize {
    use cytosol::runtime::RecordContainer;

    let id = record_id.to_id();
    cell_env.0.count_records(id)
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
    f: extern "C" fn(*mut std::os::raw::c_void, *const ValueBuffer),
    data: *mut std::os::raw::c_void,
) {
    let name = CStr::from_ptr(name);

    let ctx = exec_state.0.program_context();
    ctx.set_extern_function_raw(name.to_string_lossy().into_owned(), move |args| {
        let buf = ValueBuffer(args.to_vec());
        f(data, &buf as *const _);
    });
}
