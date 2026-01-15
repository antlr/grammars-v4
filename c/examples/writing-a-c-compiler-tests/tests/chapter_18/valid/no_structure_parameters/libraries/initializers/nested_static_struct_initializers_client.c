/* Test initialization of nested static structs, including:
 * - partial initialization
 * - arrays of structs, structs containing arrays
 * - implicit conversion of scalar elements, array decay of string literals
 */

#include "nested_static_struct_initializers.h"


#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#pragma clang diagnostic ignored "-Wimplicit-const-int-float-conversion"
#pragma clang diagnostic ignored "-Wliteral-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// structs defined here
// validation functions defined in library

// case 1: struct with no explicit initializer should be all zeros
struct outer all_zeros;

// case 2: partially initialized struct
struct outer partial = {
    100l,
    {10, {10}},  // leave arr[1], arr[2], and y uninitialized
    "Hello!"};   // leave d uninitialized

struct outer full = {
    18014398509481979l,
    {1000, "ok",
     4292870144u},  // can initialized signed char array w/ static string
    "Another message",
    2e12};

struct outer converted = {
    10.5,  // 10l
    {
        2147483650u,  // -2147483646
        {
            15.6,             // 15
            17592186044419l,  // 3
            2147483777u       // -127
        },
        1152921506754330624ul  // 2147483648u
    },
    0ul,                   // null pointer
    9223372036854776833ul  // 9223372036854777856.0
};

struct outer struct_array[3] = {{1, {2, "ab", 3}, 0, 5},
                                {6, {7, "cd", 8}, "Message", 9}};

int main(void) {
    if (!test_uninitialized()) {
        return 1;
    }

    if (!test_partially_initialized()) {
        return 2;
    }

    if (!test_fully_intialized()) {
        return 3;
    }

    if (!test_implicit_conversions()) {
        return 4;
    }

    if (!test_array_of_structs()) {
        return 5;
    }

    return 0;  // success
}