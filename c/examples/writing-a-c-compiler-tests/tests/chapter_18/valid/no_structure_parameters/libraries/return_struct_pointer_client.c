/* Test returning struct pointers from functions
 * and using struct pointers returned from functions
 * */

#include "return_struct_pointer.h"

// case 1: use a struct pointer returned from a function call
int test_get_struct_ptr(void) {
    struct inner *inner_ptr = make_struct_inner(11);

    if (inner_ptr->d != 11 || inner_ptr->i != 11) {
        return 0;
    }

    // assign struct pointer to member
    struct outermost o = {0, 0, {0, 0, {0, 0}}};
    o.nested_ptr = make_struct_outer(20);
    if (o.nested_ptr->a != 20 || o.nested_ptr->b != 21 ||
        o.nested_ptr->substruct.d != 22 || o.nested_ptr->substruct.i != 23) {
        return 0;
    }

    return 1;  // success
}

// case 2: apply member access operations to funcall expression
int test_get_struct_pointer_member(void) {
    if (make_struct_inner(2)->d != 2) {
        return 0;
    }

    if (make_struct_outer(2)->substruct.d != 4) {
        return 0;
    }

    if (make_struct_outermost(0)->nested_ptr->a != 1) {
        return 0;
    }

    return 1;  // success
}

// case 3: update static structure member through pointer returned by funcall
// f()->member = val
struct outer *get_static_struct_ptr(void) {
    static struct outer s;
    return &s;
}

int test_update_member_thru_retval(void) {
    get_static_struct_ptr()->a = 10;
    get_static_struct_ptr()->substruct.d = 20.0;

    struct outer *ptr = get_static_struct_ptr();
    if (ptr->a != 10 || ptr->substruct.d != 20.0) {
        return 0;
    }

    return 1;  // success
}

// case 4: update whole structure member through pointer returned by funcall
int test_update_nested_struct_thru_retval(void) {
    struct inner small = {12.0, 13};
    get_static_struct_ptr()->substruct = small;
    if (get_static_struct_ptr()->substruct.d != 12.0) {
        return 0;
    }

    if (get_static_struct_ptr()->substruct.i != 13) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_get_struct_ptr()) {
        return 1;
    }

    if (!test_get_struct_pointer_member()) {
        return 2;
    }

    if (!test_update_member_thru_retval()) {
        return 3;
    }

    if (!test_update_nested_struct_thru_retval()) {
        return 4;
    }

    return 0;
}