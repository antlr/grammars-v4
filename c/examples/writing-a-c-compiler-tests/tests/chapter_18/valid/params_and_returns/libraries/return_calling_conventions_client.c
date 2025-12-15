/* Test that we return a wide range of struct types according to the ABI */
#include "return_calling_conventions.h"

int main(void) {
    struct one_int_exactly one_long = {567890l};
    struct two_ints two_ints = {'_', {5, 6, 7}};
    struct int_and_xmm int_and_xmm = {'p', 4.56};

    // returning structures

    struct one_int s1 = return_int_struct();
    if (s1.i != 1 || s1.c != 2) {
        return 1;
    }

    struct twelve_bytes s2 = return_two_int_struct();
    if (s2.i != 10 || strncmp(s2.arr, "12345678", sizeof s2.arr))
        return 2;

    struct one_xmm s3 = return_double_struct();
    if (s3.d != 100.625)
        return 3;
    struct two_xmm s4 = return_two_double_struct();
    if (s4.d[0] != 8.8 || s4.d[1] != 7.8)
        return 4;

    struct xmm_and_int s5 = return_mixed();
    if (s5.dbl.d != 10.0 || strcmp(s5.c, "ab"))
        return 5;

    struct int_and_xmm s6 = return_mixed2();
    if (s6.c != 127 || s6.d != 34e43)
        return 6;

    struct memory s7 = return_on_stack();
    if (s7.d != 1.25 || strcmp(s7.c, "xy") || s7.l != 100l || s7.i != 44)
        return 7;

    s7 = pass_and_return_regs(6, 4.0, int_and_xmm, 5, two_ints, 77, one_long,
                              99);
    // something was clobbered or set incorrectly in retval
    if (s7.d || s7.c[0] || s7.c[1] || s7.c[2])
        return 8;

    // i was set to indicate problem w/ parameter passing
    if (s7.i)
        return 9;

    if (s7.l != 100)
        return 10;  // l field was clobbered or set incorrectly

    // success!
    return 0;
}