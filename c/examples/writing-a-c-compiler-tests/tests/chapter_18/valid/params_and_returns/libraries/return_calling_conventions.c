/* Test that we return a wide range of struct types according to the ABI */
#include "return_calling_conventions.h"

struct one_int return_int_struct(void) {
    struct one_int retval = {1, 2};
    return retval;
}

struct twelve_bytes return_two_int_struct(void) {
    struct twelve_bytes retval = {10, "12345678"};
    return retval;
}

struct one_xmm return_double_struct(void) {
    struct one_xmm retval = {100.625};
    return retval;
}
struct two_xmm return_two_double_struct(void) {
    struct two_xmm retval = {{8.8, 7.8}};
    return retval;
}
struct xmm_and_int return_mixed(void) {
    struct xmm_and_int retval = {{10.0}, "ab"};
    return retval;
}
struct int_and_xmm return_mixed2(void) {
    struct int_and_xmm retval = {127, 34e43};
    return retval;
}
struct memory return_on_stack(void) {
    struct memory retval = {1.25, "xy", 100l, 44};
    return retval;
}

int leaf_call(struct two_ints t_i, int c, double d) {
    // validate t_i
    if (t_i.c != '_' || t_i.arr[0] != 5 || t_i.arr[1] != 6 || t_i.arr[2] != 7) {
        return 0;
    }

    // validate c1 and d1 (originally passed in a struct int_and_xmm)
    if (c != 'p' || d != 4.56) {
        return 0;
    }
    return 1;  // success
}

struct memory pass_and_return_regs(int i, double d, struct int_and_xmm strct,
                                   int c, struct two_ints t_i, long l,
                                   struct one_int_exactly o_i_e, int c2) {
    // include a stack variable to make sure it doen't overwrite return value
    // pointer or vice versa
    char stackbytes[8] = "zyxwvut";
    struct memory retval = {0, {0, 0, 0}, 0, 0};

    // make another function call to ensure that passing parameters
    // doesn't overwrite return address in RDI or other struct eightybtes
    // passed in registers; validate t_i and strct while we're at it
    if (!leaf_call(t_i, strct.c, strct.d)) {
        retval.i = 1;
        return retval;
    }
    // validate scalar params
    if (i != 6 || d != 4.0 || c != 5 || l != 77 || c2 != 99) {
        retval.i = 2;
        return retval;
    }
    // validate remainign struct
    if (o_i_e.l != 567890) {
        retval.i = 3;
        return retval;
    }

    // validate stackbytes
    if (strcmp(stackbytes, "zyxwvut")) {
        retval.i = 4;
        return retval;
    }
    retval.l = 100;
    return retval;  // success
}