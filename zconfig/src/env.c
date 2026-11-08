#include "env.h"

extern char **environ;

long zconfig_variable_count(void) {
    long n = 0;
    while (environ[n])
        n++;
    return n;
}

const uint8_t *zconfig_variable(long i) {
    return (const uint8_t *)environ[i];
}
