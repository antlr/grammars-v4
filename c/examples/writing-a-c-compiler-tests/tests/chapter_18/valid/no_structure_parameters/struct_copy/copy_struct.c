/* Test copying a whole struct via assignment w/ TACKY Copy instruction
 * (not Load, Store, CopytoOffset, CopyFromOffset)
 * Include static and automatic objects and result of conditional expressions.
 * */

#include "structs.h"

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

// test 1: copy one struct with auto storage duration to another
int test_auto(void) {
    struct s x = {"ab", {-1, 2}};
    struct s y = {"x", {1}};
    y = x;
    if (strcmp(y.arr, "ab") || y.inner.a != -1 || y.inner.b != 2) {
        return 0;
    }

    // update a value in y, make sure it doesn't affect x
    y.inner.a = 20;
    if (y.inner.a != 20 || x.inner.a != -1) {
        return 0;
    }

    return 1;  // success
}

// test 2: copy one struct with static storage duration to another
int test_static(void) {
    static struct s x = {"ab", {1, 2}};
    static struct s y;
    y = x;
    if (strcmp(y.arr, "ab") || y.inner.a != 1 || y.inner.b != 2) {
        return 0;
    }

    return 1;  // success
}

// test 3: copy a struct w/ uneven size
struct wonky {
    char arr[7];
};

int test_wonky_size(void) {
    struct wonky x = {"abcdef"};
    static struct wonky y;
    y = x;
    if (strcmp(y.arr, "abcdef")) {
        return 0;
    }
    return 1;  // success
}

// test 4: assign result of conditional expression to struct
int true_flag(void) {
    return 1;
}

int test_conditional(void) {
    static struct s x = {"xy", {1234, 5678}};
    struct s y = {"!", {-10}};
    struct s z;
    z = true_flag() ? x : y;
    if (strcmp(z.arr, "xy") || z.inner.a != 1234 || z.inner.b != 5678) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_auto()) {
        return 1;
    }

    if (!test_static()) {
        return 2;
    }

    if (!test_wonky_size()) {
        return 3;
    }

    if (!test_conditional()) {
        return 4;
    }

    return 0;
}