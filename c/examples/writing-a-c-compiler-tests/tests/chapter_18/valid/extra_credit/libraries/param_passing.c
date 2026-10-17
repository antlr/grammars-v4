/* Test passing union types along w/ other arguments according to ABI;
 * these functions just validate params passed by client
 */
#include "union_lib.h"

int pass_unions_and_structs(int i1, int i2, struct has_union one_gp_struct,
    double d1, union two_doubles two_xmm, union one_int one_gp, int i3, int i4,
    int i5) {
    // start w/ scalars
    if (!(i1 == 1 && i2 == 2 && d1 == 4.0 && i3 == 100 && i4 == 120 && i5 == 130)) {
        return 0; // fail
    }

    // then validate structs/unions
    if (!(one_gp_struct.i == (unsigned int)-24 && one_gp_struct.u.i == 123456789)) {
        return 0; // fail
    }

    if (!(two_xmm.arr[0] == -10. && two_xmm.arr[1] == -11.)) {
        return 0; // fail
    }

    if (!(one_gp.d == 13.)) {
        return 0; // fail
    }

    return 1; // success
}

int pass_gp_union_in_memory(union two_doubles two_xmm,
    struct has_union one_gp_struct, int i1, int i2, int i3,
    int i4, int i5, int i6, union one_int one_gp) {

    // first validate scalars
    if (!(i1 == -1 && i2 == -2 && i3 == -3 && i4 == -4 && i5 == -5 && i6 == -6)) {
        return 0; // fail
    }

    // now validate structs/unions
    if (!(two_xmm.arr[0] == -10. && two_xmm.arr[1] == -11.)) {
        return 0; // fail
    }

    if (!(one_gp_struct.i == (unsigned int)-24 && one_gp_struct.u.i == 123456789)) {
        return 0; // fail
    }

    if (!(one_gp.d == 13.)) {
        return 0; // fail
    }

    return 1; // success

}

int pass_xmm_union_in_memory(double d1, double d2, union two_doubles two_xmm,
    union two_doubles two_xmm_copy, double d3, double d4,
    union two_doubles two_xmm_2) {

    // start w/ scalars
    if (!(d1 == 1.0 && d2 == 2.0 && d3 == 3.0 && d4 == 4.0)) {
        return 0;
    }

    // next validate unions
    if (!(two_xmm.arr[0] == -10. && two_xmm.arr[1] == -11.)) {
        return 0; // fail
    }

    if (!(two_xmm_copy.arr[0] == -10. && two_xmm_copy.arr[1] == -11.)) {
        return 0; // fail
    }
    if (!(two_xmm_2.arr[0] == 33e4 && two_xmm_2.arr[1] == 55e6)) {
        return 0; // fail
    }

    return 1; // success
}

int pass_borderline_union(int i1, int i2, int i3, int i4, int i5,
    union char_arr two_gp) {

    if (!(i1 == 1 && i2 == 2 && i3 == 3 && i4 == 4 && i5 == 5)) {
        return 0; // fail
    }

    if (strcmp(two_gp.arr, "+_)(*&^%$#") != 0) {
        return 0; // fail
    }

    return 1; // success
}

int pass_borderline_xmm_union(union two_doubles two_xmm, double d1, double d2,
    double d3, double d4, double d5, union two_doubles two_xmm_2) {

    // scalars first
    if (!(d1 == 9.0 && d2 == 8.0 && d3 == 7.0 && d4 == 6.0 && d5 == 5.0)) {
        return 0; // fail
    }

    // then unions
    if (!(two_xmm.arr[0] == -10. && two_xmm.arr[1] == -11.)) {
        return 0; // fail
    }

    if (!(two_xmm_2.arr[0] == 66e4 && two_xmm_2.arr[1] == 110e6)) {
        return 0;
    }
    return 1; // success
}

int pass_mixed_reg_in_memory(double d1, double d2, double d3, double d4,
    int i1, int i2, int i3, int i4, int i5, int i6,
    union gp_and_xmm mixed_regs) {

    // start w/ scalars
    if (!(d1 == 101.2 && d2 == 102.3 && d3 == 103.4 && d4 == 104.5 && i1 == 75 && i2 == 76 && i3 == 77 && i4 == 78 && i5 == 79 && i6 == 80)) {
        return 0; // fail
    }

    // then union
    if (!(mixed_regs.d_arr[0] == 0 && mixed_regs.d_arr[1] == 150.5)) {
        return 0; // fail
    }

    return 1; // success
}
int pass_uneven_union_in_memory(int i1, int i2, int i3, int i4, int i5,
    union gp_and_xmm mixed_regs, union one_int one_gp, union uneven uneven) {

    // scalars first
    if (!(i1 == 1100 && i2 == 2200 && i3 == 3300 && i4 == 4400 && i5 == 5500)) {
        return 0; // fail
    }

    // then unions
    if (!(mixed_regs.d_arr[0] == 0 && mixed_regs.d_arr[1] == 150.5)) {
        return 0; // fail
    }

    if (!(one_gp.d == 13.)) {
        return 0; // fail
    }

    if (strcmp(uneven.arr, "boop") != 0) {
        return 0; // fail
    }

    return 1; // success

}
int pass_in_mem_first(union lotsa_doubles mem, union gp_and_xmm mixed_regs,
    union char_arr two_gp, struct has_union one_gp_struct) {

    if (!(mem.arr[0] == 66. && mem.arr[1] == 77. && mem.arr[2] == 88.)) {
        return 0; // fail
    }

    if (!(mixed_regs.d_arr[0] == 0 && mixed_regs.d_arr[1] == 150.5)) {
        return 0; // fail
    }

    if (strcmp(two_gp.arr, "+_)(*&^%$#") != 0) {
        return 0; // fail
    }

    if (!(one_gp_struct.i == (unsigned int)-24 && one_gp_struct.u.i == 123456789)) {
        return 0; // fail
    }

    return 1; // success
}