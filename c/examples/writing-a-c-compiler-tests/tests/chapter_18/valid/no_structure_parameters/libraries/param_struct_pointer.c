/* Test passing struct pointers as function parameters */

#include "param_struct_pointer.h"

int access_members_through_pointer(struct outer *ptr, int expected_a,
                                   int expected_b, double expected_d,
                                   int expected_i) {
    if (ptr->a != expected_a) {
        return 0;
    }

    if (ptr->b != expected_b) {
        return 0;
    }

    if (ptr->substruct.d != expected_d) {
        return 0;
    }

    if (ptr->substruct.i != expected_i) {
        return 0;
    }

    return 1;  // success
}

void update_members_through_pointer(struct outer *ptr, int a, int b,
                                    struct inner *inner_ptr) {
    ptr->a = a;
    ptr->b = b;
    ptr->substruct = *inner_ptr;
    return;
}