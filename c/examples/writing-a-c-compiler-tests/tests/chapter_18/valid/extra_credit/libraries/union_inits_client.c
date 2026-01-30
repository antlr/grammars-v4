// Test initialization of unions with automatic storage duration

#include "union_inits.h"

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#ifdef __clang__
#pragma clang diagnostic ignored "-Wimplicit-const-int-float-conversion"
#endif
#endif

int test_simple(void) {
    // initialize simple union w/ only scalar members
    union simple x = { 123.45 };
    return validate_simple(&x);
}

int test_simple_converted(void) {
    // initialize simple union where value of element is implicitly converted
    // to target type (in this case the nearest representatble double,
    // 18446744073709549568.0)
    union simple x = { 18446744073709550315UL };
    return validate_simple_converted(&x);
}


int test_nested(void) {
    // initalize nested union where first member is a structure
    union nested x = { {4294967395l, {{-1, -2, -3, -4, -5, -6, -7, -8, -9}}} };
    return validate_nested(&x);
}

int test_nested_partial_init(void) {
    // initialize union where inner subobject is a partly initialized struct
    union nested x = { {9000372036854775800l, {"string"}} };
    return validate_nested_partial(&x);
}

int main(void) {
    if (!test_simple()) {
        return 1;
    }

    if (!test_simple_converted()) {
        return 2;
    }

    if (!test_nested()) {
        return 3;
    }

    if (!test_nested_partial_init()) {
        return 4;
    }

    return 0;
}