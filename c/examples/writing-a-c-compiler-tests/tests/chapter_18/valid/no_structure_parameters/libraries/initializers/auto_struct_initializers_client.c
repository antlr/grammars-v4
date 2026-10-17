/* Test initialization of non-nested structs with automatic storage duration,
 * including:
 * - partial initialization
 * - implicit type conversions
 * - compound and single expressions as initializers
 * - string literals as pointer and array initializers
 * */

#include "auto_struct_initializers.h"

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

double get_double(void) {
    return 2e12;
}

// case 1: fully initialized struct
int test_full_initialization(void) {
    struct s full = {
        // use string literals to initialize both pointers and arrays
        "I'm a struct!", "sup",
        &full,          // initialize member with pointer to self
        get_double(),   // initialize member with result of function call
        &(full.four_d)  // initialize member with pointer to other member in
                        // self
    };

    return validate_full_initialization(&full);
}

// case 2: partially initialized struct
int test_partial_initialization(void) {
    static char *msg = "Another string literal";
    struct s partial = {
        msg,         // initialize member from variable
        {'a', 'b'},  // partially initialize array
        (struct s *)calloc(
            1,
            sizeof(struct s))  // initialize ptr with call to calloc
        // don't initialize last element
    };

    return validate_partial_initialization(&partial, msg);
}

// case 3: implicit type conversions for struct members
int test_implicit_type_conversions(void) {
    static int i = 3000;

    struct s converted = {
        malloc(5),              // convert void * to char *
        {i / 2, i / 3, i * 4},  // truncate ints to chars: 220, 232, and 224
        0l,                     // convert null pointer constant to null pointer
        i - 1,                  // convert int to double
        calloc(1, sizeof(double))  // convert void * to double *
    };

    return validate_converted(&converted);
}

// case 4: initialize with single expression instead of compound initiailizer
int test_single_exp_initializer(void) {
    double d = 123.4;
    struct s s1 = {"Yet another string", "xy", &s1, 150.0, &d};
    struct s s2 = s1;

    return validate_two_structs(&s1, &s2);
}

int main(void) {
    if (!test_full_initialization()) {
        return 1;
    }

    if (!test_partial_initialization()) {
        return 2;
    }

    if (!test_implicit_type_conversions()) {
        return 3;
    }

    if (!test_single_exp_initializer()) {
        return 4;
    }

    return 0;  // success
}