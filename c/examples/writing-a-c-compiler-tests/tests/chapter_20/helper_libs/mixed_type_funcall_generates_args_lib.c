#include <stdio.h>
#include <stdlib.h>

struct s {
    long l;
    double d;
};

int callee(struct s s1, long a, double b) {
    if (s1.l != -50) {
        printf("Expected s1.l to be -50, found %ld\n", s1.l);
        exit(-1);
    }

    if (s1.d != -40.0) {
        printf("Expected s1.d to be -40.0, found %f\n", s1.d);
        exit(-1);
    }

    if (a != 101) {
        printf("Expected a to have value 101, actual value was %ld\n", a);
        exit(-1);
    }

    if (b != 202.0) {
        printf("Expected b to have value 202.0, actual value was %f\n", b);
        exit(-1);
    }

    return 0;
}