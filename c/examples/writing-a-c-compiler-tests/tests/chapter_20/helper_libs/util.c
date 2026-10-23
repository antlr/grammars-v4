/* Helper functions used by several tests */

// okay to use standard library b/c we compile this file with
// the system compiler, not the reader's compiler
#include <stdio.h>
#include <stdlib.h>

/* The check_* functions return 0 on success,
 * print and exit with code -1 on failure.
 */

/* Validating ints */

int check_one_int(int actual, int expected) {
    if (actual != expected) {
        printf("Expected %d but found %d\n", expected, actual);
        exit(-1);
    }
    return 0;
}

// validates a == start, b == start + 1, ...e == start + 5
// and exits early if they don't have those values
int check_5_ints(int a, int b, int c, int d, int e, int start) {
    int args[5] = {a, b, c, d, e};
    for (int i = 0; i < 5; i++) {
        int expected = start + i;
        if (args[i] != expected) {
            printf(
                "Expected argument %d to have value %d, actual value was %d\n",
                i, start + i, args[i]);
            exit(-1);
        }
    }

    return 0;  // success
}

// validates a == start, b == start + 1, ... l == start + 11
// and exits early if they don't have those values
// TODO refactor pre-chapter-20 tests that define check_12_ints to use this
// library instead?
int check_12_ints(int a, int b, int c, int d, int e, int f, int g, int h, int i,
                  int j, int k, int l, int start) {
    int args[12] = {a, b, c, d, e, f, g, h, i, j, k, l};
    for (int i = 0; i < 12; i++) {
        int expected = start + i;
        if (args[i] != expected) {
            printf(
                "Expected argument %d to have value %d, actual value was %d\n",
                i, start + i, args[i]);
            exit(-1);
        }
    }

    return 0;  // success
}

/* Validating other types */

int check_one_uchar(unsigned char actual, unsigned char expected) {
    if (actual != expected) {
        printf("Expected %c but found %c\n", expected, actual);
        exit(-1);
    }
    return 0;
}

int check_one_uint(unsigned int actual, unsigned int expected) {
    if (actual != expected) {
        printf("Expected %u but found %u\n", expected, actual);
        exit(-1);
    }
    return 0;
}

int check_one_long(long actual, long expected) {
    if (actual != expected) {
        printf("Expected %ld but found %ld\n", expected, actual);
        exit(-1);
    }
    return 0;
}

int check_one_ulong(unsigned long actual, unsigned long expected) {
    if (actual != expected) {
        printf("Expected %lu but found %lu\n", expected, actual);
        exit(-1);
    }
    return 0;
}

int check_one_double(double actual, double expected) {
    if (actual != expected) {
        printf("Expected %f but found %f\n", expected, actual);
        exit(-1);
    }
    return 0;
}

// identical to check_12_ints, but w/ longs
int check_12_longs(long a, long b, long c, long d, long e, long f, long g,
                   long h, long i, long j, long k, long l, long start) {
    // validate that a == start, b == start + 1, ...l == start + 11
    long args[12] = {a, b, c, d, e, f, g, h, i, j, k, l};
    for (int i = 0; i < 12; i++) {
        long expected = start + i;
        if (args[i] != expected) {
            printf(
                "Expected argument %d to have value %ld, actual value was "
                "%ld\n",
                i, start + i, args[i]);
            exit(-1);
        }
    }

    return 0;  // success
}

// similar to check_5_ints but with 6 chars;
// validates a == start, b == start + 1, ...f == start + 6
// and exits early if they don't have those values
int check_six_chars(char a, char b, char c, char d, char e, char f, int start) {
    char args[6] = {a, b, c, d, e, f};
    for (int i = 0; i < 6; i++) {
        int expected = start + i;
        if (args[i] != expected) {
            printf(
                "Expected argument %d to have value %d, actual value was %d\n",
                i, start + i, args[i]);
            exit(-1);
        }
    }

    return 0;  // success
}

// validates a == start, b == start + 1, ... n == start + 13
// and exits early if they don't have those values
// NOTE: assumes a-n are small integral values that can be represented exactly
// as double so no rounding error
int check_14_doubles(double a, double b, double c, double d, double e, double f,
                     double g, double h, double i, double j, double k, double l,
                     double m, double n, double start) {
    double args[14] = {a, b, c, d, e, f, g, h, i, j, k, l, m, n};
    for (int i = 0; i < 14; i++) {
        double expected = start + i;
        if (args[i] != expected) {
            printf(
                "Expected argument %d to have value %f, actual value was %f\n",
                i, start + i, args[i]);
            exit(-1);
        }
    }
    return 0;  // success
}

// Used in force_spill_mixed_ints; validates a == start, b == start + 1, ...,
// *k == start + 10, *l == start + 11,
// and exits early if they don't have those values
int check_12_vals(int a, int b, int c, int d, int e, int f, int g, int h, int i,
                  int j, long* k, double* l, int start) {
    int args[10] = {a, b, c, d, e, f, g, h, i, j};
    for (int i = 0; i < 10; i++) {
        int expected = start + i;
        if (args[i] != expected) {
            printf(
                "Expected argument %d to have value %d, actual value was %d\n",
                i, expected, args[i]);
            exit(-1);
        }
    }

    if (*k != start + 10) {
        printf("Expected *k to point to have value %d, actual value was %ld\n",
               start + 10, *k);
        exit(-1);
    }

    if (*l != start + 11) {
        printf("Expected *l to point to have value %d, actual value was %f\n",
               start + 11, *l);
        exit(-1);
    }

    return 0;  // success
}

/* Identity functions that return x;
 * used to get constants in a way that can't be optimized away
 */

int id(int x) {
    return x;
}

double dbl_id(double x) {
    return x;
}

long long_id(long l) {
    return l;
}

unsigned unsigned_id(unsigned u) {
    return u;
}

unsigned char uchar_id(unsigned char uc) {
    return uc;
}