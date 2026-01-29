// Test initialization of unions with both automatic and static storage duration

#include "union_inits.h"

int validate_simple(union simple* ptr) {
    return (ptr->d == 123.45);
}

int validate_simple_converted(union simple* ptr) {
    return (ptr->d == 18446744073709549568.);
}

int validate_nested(union nested* ptr) {
    if (ptr->str.l != 4294967395l) {
        return 0; // fail
    }

    for (int i = 0; i < 9; i = i + 1) {
        if (ptr->str.u.arr[i] != -1 - i) {
            return 0;  // fail
        }
    }

    return 1; // success
}
int validate_nested_partial(union nested* ptr) {
    if (ptr->str.l != 9000372036854775800l) {
        return 0; // fail
    }

    if (strcmp(ptr->str.u.arr, "string")) {
        return 0; // fail
    }

    return 1; // success
}