#ifndef UTIL_ENV
#define UTIL_ENV

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#define N_default 7
typedef struct
{
    const char *key;   // Name of the environment variable
    const char *value; // File suffix to append
} SuffixMapping;

void x_set_env(const char *var, const char *value)
{
#ifdef _WIN32
    // Calculate length for the "VAR=VALUE" format
    size_t len = strlen(var) + strlen(value) + 2; // "VAR=VALUE" + null terminator

    // Static array, large enough to hold the "VAR=VALUE" string
    char env_str[len];

    // Create the environment string in the "VAR=VALUE" format
    snprintf(env_str, sizeof(env_str), "%s=%s", var, value);
    _putenv(env_str); // Windows: _putenv requires a persistent string
#else
    setenv(var, value, 1); // Linux: setenv doesn't need persistent storage
#endif
}

void set_runtime_path(char *path)
{

#ifdef _WIN32
    DWORD len = GetModuleFileName(NULL, path, sizeof(path) - 1);
    if (len == 0)
    {
        fprintf(stderr, "Error getting executable path on Windows.\n");
    }
#else
    ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (len == -1)
    {
        perror("readlink");
    }
#endif
    path[len] = '\0';
}

void set_default_runtime_env()
{
    //-- Set the runtime path of executable
    char path[1024];
    set_runtime_path(path);

    //-- Set required environment variables for the runtime path
    SuffixMapping default_mappings[N_default] = {
        {"OASES_SH", path},
        {"OASES_BIN", path},
        {"OASES_LIB", path},
        {"PATH", path},
        {"USRTERMTYPE", "x"},
        {"CON_PACKGE", "MTV"},
        {"PLP_PACKGE", "MTV"}};

    //-- Update environment variables for the run time path and current library
    for (int i = 0; i < N_default; i++)
        x_set_env(default_mappings[i].key, default_mappings[i].value);
}
#endif // UTIL_ENV