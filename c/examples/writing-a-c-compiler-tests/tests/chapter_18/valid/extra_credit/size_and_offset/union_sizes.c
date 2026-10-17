// Apply sizeof to a range of union types
#include "../../no_structure_parameters/size_and_offset_calculations/struct_sizes.h"

// size is 11 bytes; no padding
union no_padding {
    char c;
    unsigned char uc;
    signed char arr[11];
};


// size is 12 bytes; take largest member (10 bytes)
// and pad to 4-byte alignment (b/c ui is 4-byte aligned)
union with_padding {
    signed char arr[10];
    unsigned int ui;
};


// size is 36 bytes
// arr1 is 24 bytes, 4-byte aligned
// arr2 is 33 bytes, 1-byte aligned
// round 33 up to multiple of 4 to get 36
union contains_array {
    union with_padding arr1[2];
    union no_padding arr[3];
};


// 8 bytes, no padding
union double_and_int {
    int i;
    double d;
};

// 20 bytes, 4-byte aligned
union contains_structs {
    struct wonky x; // 19 bytes, 1-byte aligned
    struct eight_bytes y; // 8 bytes, 4-byte aligned
};

int main(void) {
    if (sizeof(union no_padding) != 11) {
        return 1; // fail
    }

    if (sizeof(union with_padding) != 12) {
        return 2; // fail
    }

    if (sizeof(union contains_array) != 36) {
        return 3; // fail
    }

    if (sizeof(union double_and_int) != 8) {
        return 4; // fail
    }

    if (sizeof(union contains_structs) != 20) {
        return 5; // fail
    }


    // apply sizeof to some expressions with union type too
    union no_padding x = { 1 };
    union contains_array y = { {{{-1, 2}} }};
    union contains_structs* get_union_ptr(void);

    if (sizeof x != 11) {
        return 6; // fail
    }

    if (sizeof y.arr1 != 24) { // array of two union with_padding objects
        return 7; // fail
    }

    if (sizeof * get_union_ptr() != 20) {
        return 8; // fail
    }


    return 0; // success
}

union contains_structs* get_union_ptr(void) {
    // just return null pointer - okay b/c we never actually access this struct
    return 0;
}