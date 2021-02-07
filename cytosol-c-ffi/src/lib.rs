use std::ffi::CStr;

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

//
// Values
//

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
