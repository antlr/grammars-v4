// Helper functions for
// tests/chapter_20/all_types/no_coalescing/mixed_type_arg_registers.c

#include <stdio.h>
#include <stdlib.h>

struct s1 {
    // XMM0
    double d;
    // RDI
    char c;
    int i;
};

struct s2 {
    // RSI
    unsigned long ul;
    // XMM1
    double d;
};

// passed in memory
struct s3 {
    double d1;
    double d2;
    signed char s;
};

int callee(struct s1 a, struct s2 b, char c, struct s3 in_mem) {
    if (a.d != 11.0) {
        printf("Expected a.d to have value 11.0, actual value was %f\n", a.d);
    }

    if (a.c != 8) {
        printf("Expected a.c to have value 8, actual value was %d\n", (int)a.c);
    }

    if (a.i != 9) {
        printf("Expected a.i to have value 9, actual value was %d\n", a.i);
    }

    if (b.ul != 10ul) {
        printf("Expected b.ul to have value 10, actual value was %lu\n", b.ul);
    }

    if (b.d != 12.0) {
        printf("Expected b.d to have value 12.0, actual value was %f\n", b.d);
    }

    if (c != 12) {
        printf("Expected c to have value 12, actual value was %d\n", (int)c);
    }

    if (in_mem.d1 != 13.0) {
        printf("Expected in_mem.d1 to have value 13.0, actual value was %f\n",
               in_mem.d1);
    }

    if (in_mem.d2 != 14.0) {
        printf("Expected in_mem.d2 to have value 14.0, actual value was %f\n",
               in_mem.d2);
    }
    if (in_mem.s != 11) {
        printf("Expected in_mem.s to have value 11, actual value was %d\n",
               (int)in_mem.s);
    }

    return 0;
}

int check_some_args(int one, long two, unsigned int three, unsigned long four,
                    char five, unsigned char six, signed char seven) {
    if (one != 1) {
        printf("Expected one to have value 1, actual value was %d\n", one);
        exit(-1);
    }

    if (two != 2l) {
        printf("Expected two to have value 2, actual value was %ld\n", two);
        exit(-1);
    }

    if (three != 3u) {
        printf("Expected three to have value 3, actual value was %u\n", three);
        exit(-1);
    }

    if (four != 4ul) {
        printf("Expected four to have value 4, actual value was %lu\n", four);
        exit(-1);
    }

    if (five != 5) {
        printf("Expected five to have value 5, actual value was %d\n",
               (int)five);
        exit(-1);
    }

    if (six != 6) {
        printf("Expected six to have value 6, actual value was %d\n", (int)six);
        exit(-1);
    }

    if (seven != 7) {
        printf("Expected seven to have value 7, actual value was %d\n",
               (int)seven);
        exit(-1);
    }

    return 0;
}
