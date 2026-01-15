/* Test initialization of non-nested static structs, including:
 * - partial initialization
 * - implicit conversion of scalar elements
 * - array decay of string literals
 */

#include "static_struct_initializers.h"

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#pragma clang diagnostic ignored "-Wimplicit-const-int-float-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// case 1: struct with no explicit initializer should be all zeros
struct s uninitialized;

// case 2: partially initialized struct
struct s partial = {1.0, "Hello"};

// case 3: partially initialized array w/in struct
struct s partial_with_array = {3.0, "!", {1}, 2};

// case 4: implicit conversion of scalar elements
struct s converted = {
    1152921504606846977l,  // 1152921504606846976.0
    0l,                    // null ptr
    "abc",                 // {'a', 'b', 'c'}
    17179869189l           // 5
};

int main(void) {
    if (!test_uninitialized()) {
        return 1;
    }

    if (!test_partially_initialized()) {
        return 2;
    }

    if (!test_partial_inner_init()) {
        return 3;
    }

    if (!test_implicit_conversion()) {
        return 4;
    }

    return 0;  // success
}
