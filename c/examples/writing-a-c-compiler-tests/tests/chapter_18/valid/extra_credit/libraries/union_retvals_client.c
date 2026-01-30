/* Test returning unions (and structs containing unions) according to the ABI */

#include "union_lib.h"

int main(void) {

    // return a value in one XMM register
    union one_double od = return_one_double();
    if (!(od.d1 == 245.5 && od.d2 == 245.5)) {
        return 1; // fail
    }

    // return a value in one general-purpose register
    union one_int_nested oin = return_one_int_nested();
    if (oin.oi.d != -9876.5) {
        return 2; // fail
    }

    // return a value in two XMM registers
    union has_dbl_struct two_xmm = return_has_dbl_struct();
    if (!(two_xmm.member1.member1.d1 == 1234.5 && two_xmm.member1.member2 == 6789.)) {
        return 3; // fail
    }

    // return a value in two general-purpose registers
    union two_arrs two_arrs = return_two_arrs();
    if (two_arrs.dbl_arr[0] != 66.75 || two_arrs.long_arr[1] != -4294967300l) {
        return 4;
    }

    // return a value in one general-purpose and one XMM register
    union scalar_and_struct int_and_dbl = return_scalar_and_struct();
    if (int_and_dbl.cfe.c != -115 || int_and_dbl.cfe.d != 222222.25) {
        return 5;
    }

    // return a value in one XMM and one general-purpose register
    union xmm_and_gp dbl_and_int = return_xmm_and_gp();
    if (dbl_and_int.d != -50000.125 || dbl_and_int.ise.d != -50000.125
        || dbl_and_int.ise.i != -3000) {
        return 6;
    }

    // return a value in memory
    union contains_union_array big_union = return_contains_union_array();
    if (!(big_union.arr[0].d_arr[0] == -2000e-4 && big_union.arr[0].d_arr[1] == -3000e-4
        && big_union.arr[1].d_arr[0] == 20000e10 && big_union.arr[1].d_arr[1] == 5000e11)) {
        return 7;
    }

    // pass some unions and return a value in memory;
    // make sure returning in memory doesn't screw up param passing
    union lotsa_chars chars_union = pass_params_and_return_in_mem(1,
        int_and_dbl, two_arrs, 25, big_union, oin);

    if (strcmp(chars_union.more_chars, "ABCDEFGHIJKLMNOPQ") != 0) {
        return 8;
    }

    // return a struct that contains a union (in two registers)
    struct has_uneven_union s = return_struct_with_union();
    if (s.i != -8765 || strcmp(s.u.arr, "done") != 0) {
        return 9;
    }

    return 0; // success!
}