// Test copying whole structs/unions through pointers (incl. to/from array members)

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

#include "../union_types.h"

int strcmp(char* s1, char* s2);

// case 1: *x = y
int test_copy_to_pointer(void) {
    union simple y;
    y.l = -20;
    union simple* x = malloc(sizeof(union simple));
    *x = y;

    // validate
    if (x->l != -20 || x->i != -20 || x->uc_arr[0] != 236 || x->uc_arr[1] != 255 || x->uc_arr[2] != 255) {
        return 0; // fail
    }

    return 1;  // success
}

// case 2: x = *y
int test_copy_from_pointer(void) {
    // define/initialize a union object containing a struct
    struct simple_struct my_struct = { 8223372036854775807l, 20e3, 2147483650u };
    static union has_struct my_union;
    my_union.s = my_struct;

    // get a pointer to that union
    union has_struct* union_ptr;
    union_ptr = &my_union;

    // copy from pointer to another union
    union has_struct another_union = *union_ptr;

    // validate
    if (another_union.s.l != 8223372036854775807l || another_union.s.d != 20e3 || another_union.s.u != 2147483650u) {
        return 0; // fail
    }

    return 1;
}

// case 3: copies to and from array members (using a union w/ trailing padding)

// size is 12 bytes; take largest member (10 bytes)
// and pad to 4-byte alignment (b/c ui is 4-byte aligned)
union with_padding {
    char arr[10];
    unsigned int ui;
};

int test_copy_array_members(void) {

    // define/initialize an array of unions
    union with_padding union_array[3] = { {"foobar"}, {"hello"}, {"itsaunion"} };

    // copy element out of array
    union with_padding another_union = union_array[0];
    union with_padding yet_another_union = { "blahblah" };

    // copy an element into the array
    union_array[2] = yet_another_union;

    // validate
    if (strcmp(union_array[0].arr, "foobar") || strcmp(union_array[1].arr, "hello") || strcmp(union_array[2].arr, "blahblah")) {
        return 0; // fail
    }

    if (strcmp(another_union.arr, "foobar")) {
        return 0; // fail
    }

    // check yet_another_union too, even though we didn't update it
    if (strcmp(yet_another_union.arr, "blahblah")) {
        return 0; // fail
    }

    return 1; // success

}

int main(void) {
    if (!test_copy_to_pointer()){
        return 1;
    }

    if (!test_copy_from_pointer()) {
        return 2;
    }

    if (!test_copy_array_members()) {
        return 3;
    }

    return 0; // success
}