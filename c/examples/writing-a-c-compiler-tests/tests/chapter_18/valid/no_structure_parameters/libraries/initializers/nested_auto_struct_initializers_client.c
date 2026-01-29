/* Test initialization of nested structs with automatic storage duration,
 * including:
 * - partial initialization
 * - using mix of compound and single initializers to initialize nested structs
 * - arrays of structs, structs containing arrays
 * */

#include "nested_auto_struct_initializers.h"

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
#endif
#endif

// case 1: fully initialized struct (include some implicit conversions while
// we're at it)
int test_full_initialization(void) {
    struct outer full = {-200,
                         {-171l, {-56, -54, -53}, 40.5},
                         "Important message!",
                         -22,
                         {1, 2}};

    return validate_full_initialization(&full);
}

// case 2: partially initialized struct
int test_partial_initialization(void) {
    struct outer partial = {1000,
                            {
                                1,
                                // leave two_arr and three_u ininitialized
                            },
                            "Partial"};  // leave four_d uninitialized

    return validate_partial_initialization(&partial);
}

// case 3: initialize a nested struct with a single expression of struct type
// rather than a compound initializer
int test_mixed_initialization(void) {
    struct inner inner1 = {10};
    struct inner inner2 = {20, {21}, 22u};
    static int flag = 0;

    struct outer mixed = {
        200,
        flag ? inner1 : inner2,  // initialize to inner2
        "mixed",
        10.0,
        {99,
         100}  // still use compound init for second nexted struct, five_pair
    };

    return validate_mixed_initialization(&mixed);
}

// case 4: initialize an array of structures
int test_array_of_structs(void) {
    struct outer s0 = {1, {2, {3, 4, 5}, 6}, "7", 8.0, {9, 10}};

    struct inner in1 = {102, {103, 104, 105}, 106};
    struct pair pair1 = {109, 110};
    struct pair pair2 = {209};

    struct outer s3 = {301};

    struct outer struct_array[4] = {
        // struct_array[0]: initialize whole array element w/ one struct
        s0,
        {101, in1, "107", 108.0, pair1},
        // struct_array[2]: partial initialization; compound initialize for one
        // subelement, single for other
        {201,
         // struct_array[2].two_struct
         {202, {203}},
         "207",
         208.0,
         pair2},
        // struct_array[3]: initialize whole array element from one partially
        // initialized struct
        s3};

    return validate_array_of_structs(struct_array);
}

int main(void) {
    if (!test_full_initialization()) {
        return 1;
    }

    if (!test_partial_initialization()) {
        return 2;
    }

    if (!test_mixed_initialization()) {
        return 3;
    }

    if (!test_array_of_structs()) {
        return 4;
    }

    return 0;  // success
}