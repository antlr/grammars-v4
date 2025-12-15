/* Helper functions for funcall_generates_args.c and
 * dbl_funcall_generates_args.c
 * */

#include <stdio.h>
#include <stdlib.h>

// a and b should be 11 and 12
int f(int a, int b) {
    if (a != 11) {
        printf("Expected a to be 11 but found %d\n", a);
        exit(-1);
    }

    if (b != 12) {
        printf("Expected b to be 12 but found %d\n", b);
        exit(-1);
    }

    return 0;
}

int use_dbls(double a, double b) {
    if (a != 11.0) {
        printf("Expected a to be 11.0 but found %f\n", a);
        exit(-1);
    }

    if (b != 12.0) {
        printf("Expected b to be 12.0 but found %f\n", b);
        exit(-1);
    }

    return 0;
}