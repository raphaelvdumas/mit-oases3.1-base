#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "util_env.h"

// #!/bin/csh
// setenv FOR001 $1.dat
// setenv FOR019 $1.plp
// setenv FOR020 $1.plt
// setenv FOR026 $1.chk
// setenv FOR028 $1.cdr
// setenv FOR029 $1.bdr
// setenv FOR013 $2.rpo
// setenv FOR015 $3.xsm
// mfp2_bin

// #-- Content of oasr.sh
// #!/bin/csh
#define N 8
size_t iarg[N] = {1, 1, 1, 1, 1, 1, 2, 3};
SuffixMapping m[N] = {
    {"FOR001", ".dat"}, // setenv FOR001 $1.dat
    {"FOR019", ".plp"}, // setenv FOR019 $1.plp
    {"FOR020", ".plt"}, // setenv FOR020 $1.plt
    {"FOR026", ".chk"}, // setenv FOR026 $1.chk
    {"FOR028", ".cdr"}, // setenv FOR028 $1.cdr
    {"FOR029", ".bdr"}, // setenv FOR029 $1.bdr
    {"FOR013", ".rpo"}, // setenv FOR013 $2.rpo
    {"FOR015", ".xsm"}  // setenv FOR015 $3.xsm
};

// Linked to SUBROUTINE OASRC
extern void oaspen_();

int main(int argc, char *argv[])
{
    if (argc != 4)
    {
        fprintf(stderr, "Usage: %s <first_prefix> <second_prefix> <third_prefix>\n", argv[0]);
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

    oaspen_();
    return EXIT_SUCCESS;
}