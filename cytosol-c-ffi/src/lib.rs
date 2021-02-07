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
