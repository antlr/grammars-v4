/* Modify a parameter of structure type
 * */

#include "modify_param.h"

int modify_simple_struct(struct inner s) {
    // copy it
    struct inner copy = s;

    // modify it
    s.d = 0.0;

    // check its value
    if (s.d || s.i != 3) {
        return 0;
    }

    // check value of copy
    if (copy.d != 2.0 || copy.i != 3) {
        return 0;
    }

    return 1;  // success
}

int modify_nested_struct(struct outer s) {
    // copy it
    struct outer copy = s;

    // modify it
    s.l = 10;
    s.s.i = 200;
    s.ptr->d = 10.0;
    s.ptr->i = 11;

    // check its value
    if (s.s.i != 200 || s.s.d != 4.0 || s.l != 10 || s.ptr->d != 10.0 ||
        s.ptr->i != 11) {
        return 0;
    }

    // check value of copy
    if (copy.s.i != 5 || copy.s.d != 4.0 || copy.l != 1000 ||
        copy.ptr->d != 10.0 || copy.ptr->i != 11) {
        return 0;
    }

    return 1;  // success
}