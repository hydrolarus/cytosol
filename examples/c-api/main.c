#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "cytosol.h"

void print_string(void *data, const cyt_value_buffer *args)
{
    const char *ptr;
    size_t len;
    cyt_value_buffer_get_string(args, 0, &ptr, &len);
    printf("%.*s", (int)len, ptr);
}

int main(void)
{
    cyt_program *prog = cyt_program_new();
    cyt_driver_runner *runner = cyt_driver_runner_new();

    cyt_driver_runner_add_file_from_path(runner, "test.cyt");

    bool success = cyt_driver_runner_compile(runner, prog);
    if (!success)
    {
        cyt_driver_runner_destroy(runner);
        cyt_program_destroy(prog);
        return 1;
    }

    cyt_exec_state *es = cyt_exec_state_new();
    cyt_cell_env *env = cyt_cellenv_new();

    cyt_exec_state_set_extern_function(es, "print_string", print_string, 0);

    cyt_record_id start_id;
    assert(cyt_program_record_by_name(prog, "Start", &start_id) == true);

    // add a `Start` record into the environment to run the entry point
    cyt_cellenv_add_record(env, 1, start_id, cyt_value_buffer_new(0));

    cyt_driver_runner_run(runner, prog, es, env, 1);

    cyt_cellenv_destroy(env);
    cyt_exec_state_destroy(es);

    cyt_driver_runner_destroy(runner);
    cyt_program_destroy(prog);

    return 0;
}