#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
int main(void) {
    int outer = 1;
    int foo = 0;
    if (outer) {
        /* You can declare a variable with linkage
         * multiple times in the same block;
         * these both refer to the 'foo' variable defined below
         */
        extern int foo;
        extern int foo;
        return foo;
    }
    return 0;
}

int foo = 3;