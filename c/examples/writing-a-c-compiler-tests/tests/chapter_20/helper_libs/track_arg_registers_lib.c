#include <stdio.h>
#include <stdlib.h>

int callee(int a, int b, int c) {
    if (a != 10) {
        printf("Expected a to be 10, found %d\n", a);
        exit(-1);
    }
    if (b != 11) {
        printf("Expected b to be 11, found %d\n", b);
        exit(-1);
    }
    if (c != 12) {
        printf("Expected c to be 12, found %d\n", c);
        exit(-1);
    }
    return 0;
}