# build the example
c-api: main.c test.cyt ../../target/debug/libcytosol_c_ffi.a
	clang -o c-api main.c -I../../cytosol-c-ffi ../../target/debug/libcytosol_c_ffi.a -pthreads -ldl

# build the cytosol C API
../../target/debug/libcytosol_c_ffi.a:
	cd ../..; cargo build -p cytosol-c-ffi