/* Test returning unions (and structs containing unions) according to the ABI */

#include "union_lib.h"

union one_double return_one_double(void) {
    union one_double result = { 245.5 };
    return result;
}

union one_int_nested return_one_int_nested(void) {
    union one_int_nested result = { {-9876.5} };
    return result;
}

union has_dbl_struct return_has_dbl_struct(void) {
    union has_dbl_struct result = {
        {
            {1234.5}, 6789.
        }
    };
    return result;
}

union two_arrs return_two_arrs(void) {
    union two_arrs result;
    result.dbl_arr[0] = 66.75;
    result.long_arr[1] = -4294967300l;
    return result;
}

union scalar_and_struct return_scalar_and_struct(void) {
    union scalar_and_struct result;
    result.cfe.c = -115;
    result.cfe.d =  222222.25;
    return result;
}

union xmm_and_gp return_xmm_and_gp(void) {
    union xmm_and_gp result;
    result.ise.d = -50000.125;
    result.ise.i = -3000;
    return result;
}

union contains_union_array return_contains_union_array(void) {
    union contains_union_array result = {
        {
            {{-2000e-4, -3000e-4}}, {{20000e10, 5000e11}}
        }
    };
    return result;
}

union lotsa_chars pass_params_and_return_in_mem(int i1,
    union scalar_and_struct int_and_dbl, union two_arrs two_arrs, int i2,
    union contains_union_array big_union, union one_int_nested oin) {

    // first, validate params, starting w/ scalars
    if (i1 != 1 || i2 != 25) {
        exit(-1);
    }

    // now validate non-scalar params
    if (int_and_dbl.cfe.c != -115 || int_and_dbl.cfe.d != 222222.25) {
        exit(-2);
    }

    if (two_arrs.dbl_arr[0] != 66.75 || two_arrs.long_arr[1] != -4294967300l) {
        exit(-3);
    }

    if (!(big_union.arr[0].d_arr[0] == -2000e-4 && big_union.arr[0].d_arr[1] == -3000e-4
        && big_union.arr[1].d_arr[0] == 20000e10 && big_union.arr[1].d_arr[1] == 5000e11)) {
        exit(-4);
    }

    if (oin.oi.d != -9876.5) {
        exit(-5);
    }

    // now construct result
    union lotsa_chars result = { "ABCDEFGHIJKLMNOPQ" };
    return result;
}

struct has_uneven_union return_struct_with_union(void) {
    struct has_uneven_union result = {
        -8765, {"done"}
    };
    return result;
}