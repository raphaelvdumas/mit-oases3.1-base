#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "util_env.h"

//-- Strictly equivalent in C to oast.csh
#define N 10
size_t iarg[N] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
SuffixMapping m[N] = {
    {"FOR001", ".dat"}, // setenv FOR001 $1.dat
    {"FOR002", ".src"}, // setenv FOR002 $1.src
    {"FOR023", ".trc"}, // setenv FOR023 $1.trc
    {"FOR019", ".plp"}, // setenv FOR019 $1.plp
    {"FOR020", ".plt"}, // setenv FOR020 $1.plt
    {"FOR028", ".cdr"}, // setenv FOR028 $1.cdr
    {"FOR029", ".bdr"}, // setenv FOR029 $1.bdr
    {"FOR045", ".rhs"}, // setenv FOR045 $1.rhs
    {"FOR046", ".pat"}, // setenv FOR046 $1.pat
    {"FOR060", ".sck"}  // setenv FOR060 $1.sck
};
// Linked to SUBROUTINE oastl_
extern void oastl_(); // oast2 (entry point to oastl_)

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <filename_prefix>\n", argv[0]);
        return EXIT_FAILURE;
    }

    //-- Set default run time path ('PATH', 'OASES_SH', etc)
    set_default_runtime_env();

    //-- Set environment variables specific to subroutine
    for (int i = 0; i < N; i++)
    {
        //-- In case a long string
        char value[1024];

        //-- Copy with input argument
        snprintf(value, sizeof(value), "%s%s", argv[iarg[i]], m[i].value);

        //-- Set to environment (visible by Fortran)
        x_set_env(m[i].key, value);
    }

    // Call required Fortran subroutine
    oastl_();
    return EXIT_SUCCESS;
}