/* Test for accessing the members in a return value of structure type */
#include "access_retval_members.h"

struct inner return_small_struct(void) {
    struct inner i = {101, 102};
    return i;
}

struct outer return_nested_struct(void) {
    static struct outer ret = {2.0, 0, {10, 11}};

    // on first call to this function, initializer ret.ptr
    if (!ret.ptr) {
        ret.ptr = calloc(1, sizeof(struct inner));
        ret.ptr->x = 12;
        ret.ptr->y = 13;
    }

    return ret;
}
