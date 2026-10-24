#include <stdio.h>
#include <stdlib.h>

int callee(double a, double b, double c) {
    if (a != 12) {
        printf("Expected a to be 12, found %f\n", a);
        exit(-1);
    }
    if (b != 13) {
        printf("Expected b to be 13, found %f\n", b);
        exit(-1);
    }
    if (c != 14) {
        printf("Expected c to be 14, found %f\n", c);
        exit(-1);
    }
    return 0;
}