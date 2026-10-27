// Test initialization of static unions; make sure uninitialized
// unions/sub-objects are initialized to zero
#include "static_union_inits.h"

// Test case 1 - simple union w/ scalar elements

union simple s = {217};

// Test case 2 - union w/ another union as first element

union has_union h = {{77}};

// Test case 3 - struct containing partially initialized array of unions
// (make sure we initialize uninitialized values to zero)

struct has_union_array my_struct = {
    {{{'a'}}, {{'b'}}, {{'c'}}}, '#', {'!'}
};

// Test case 4 - uninitialized union (make sure whole thing is initialized to
// 0, not just first element)

union has_union all_zeros;

// Test case 5 - an array of unions with trailing padding. Make sure padding
// is included
union with_padding padded_union_array[3] = {
    {"first string"}, {"string #2"}, {
        "string #3"
    }
};

int main(void) {
    if (!validate_simple()) {
        return 1;
    }

    if (!validate_has_union()){
        return 2;
    }

    if (!validate_has_union_array()) {
        return 3;
    }

    if (!validate_uninitialized()) {
        return 4;
    }

    if (!validate_padded_union_array()) {
        return 5;
    }

    return 0;
}