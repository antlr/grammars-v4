#include "union_lib.h"

int main(void) {

    // Ia. passed in one XMM reg

    union one_double od = { -2.345e6 };
    if (!test_one_double(od)) {
        return 1;
    }

    struct has_union_with_double huwd = { {9887.54321e44} };
    if (!test_has_union_with_double(huwd)) {
        return 2;
    }

    union has_struct_with_double hswd = { huwd };
    if (!test_has_struct_with_double(hswd)) {
        return 3;
    }

    // IIb. passed in one general-purpose register
    union one_int oi = { -80. };
    if (!test_one_int(oi)) {
        return 4;
    }

    union one_int_nested oin = { {44e55} };
    if (!test_one_int_nested(oin)) {
        return 5;
    }

    union char_int_mixed cim = { "WXYZ" };
    if (!test_char_int_mixed(cim)) {
        return 6;
    }

    struct has_union hu = { 4294954951u, {-60} };
    if (!test_has_union(hu)) {
        return 7;
    }

    union has_struct_with_ints hswi;
    hswi.s = hu;
    if (!test_has_struct_with_ints(hswi)) {
        return 8;
    }

    // IIa. two XMM regs
    union two_doubles td = { {10.0, 11.0} };
    if (!test_two_doubles(td)) {
        return 9;
    }

    union has_xmm_union hxu;
    hxu.u2 = td;
    if (!test_has_xmm_union(hxu)) {
        return 10;
    }

    struct dbl_struct ds = { od, 123.45 };
    if (!test_dbl_struct(ds)) {
        return 11;
    }

    union has_dbl_struct hds = { ds };
    if (!test_has_dbl_struct(hds)) {
        return 12;
    }

    // IIb. two general-purpose regs
    union char_arr ca = { "Chars!" };
    if (!test_char_arr(ca)) {
        return 13;
    }

    union two_arrs two_arr_var = { {13e4, 14.5} };
    if (!test_two_arrs(two_arr_var)) {
        return 14;
    }

    union two_eightbyte_has_struct tehs = { {100, 200, 300} };
    if (!test_two_eightbyte_has_struct(tehs)) {
        return 15;
    }

    union two_structs  ts = { {'x', 55.5e5} };

    if (!test_two_structs(ts)) {
        return 16;
    }

    union has_nine_byte_struct hnbs;
    hnbs.s.i = -16711936;
    for (int i = 0; i < 5; i = i + 1) {
        char byte = i % 2 ? -1 : 0;
        hnbs.s.arr[i] = byte;
    }
    hnbs.s.arr[4] = 0;
    if (!test_has_nine_byte_struct(hnbs)) {
        return 17;
    }

    struct has_uneven_union huu = { -2147483647, {"!@#$"} };
    if (!test_has_uneven_union(huu)) {
        return 18;
    }

    union has_other_unions hou;
    hou.n = hnbs;
    hou.n.s.arr[4] = 0;
    if (!test_has_other_unions(hou)) {
        return 19;
    }

    union union_array ua = { {{-20.}, {-30.}} };
    if (!test_union_array(ua)) {
        return 20;
    }

    union uneven_union_array uua = { {{"QWER"},{"TYUI"}} };
    if (!test_uneven_union_array(uua)) {
        return 21;
    }

    union has_small_struct_array hssa = { {
        {"AS", 10}, {"DF", 11}, {"GH", 12}
    } };
    if (!test_has_small_struct_array(hssa)) {
        return 22;
    }

    // IIc. general-purpose & XMM
    union gp_and_xmm gax = { {11., 12} };
    if (!test_gp_and_xmm(gax)) {
        return 23;
    }

    union scalar_and_struct sas;
    sas.cfe.c = -5;
    sas.cfe.d = -88.8;
    if (!test_scalar_and_struct(sas)) {
        return 24;
    }

    struct has_two_unions htu = {
        cim, od
    };

    if (!test_has_two_unions(htu)) {
        return 25;
    }

    union small_struct_arr_and_dbl ssaad;
    ssaad.d.arr[0] = -22.;
    ssaad.d.arr[1] = -32.;

    if (!test_small_struct_arr_and_dbl(ssaad)) {
        return 26;
    }

    // IId. XMM & general-purpose
    union xmm_and_gp xag;
    xag.ise.d = -8.;
    xag.ise.i = -8;

    if (!test_xmm_and_gp(xag)) {
        return 27;
    }

    union xmm_and_gp_nested xagn = { xag };
    if (!test_xmm_and_gp_nested(xagn)) {
        return 28;
    }

    // III. passed in memory
    union lotsa_doubles dbls = { {99., 98., 97.} };
    if (!test_lotsa_doubles(dbls)) {
        return 29;
    }

    union lotsa_chars chars = { "asflakjsdflkjs" };
    if (!test_lotsa_chars(chars)) {
        return 30;
    }

    struct large large_struct = { 100, 100., "A struct!" };
    union contains_large_struct cls;
    cls.l = large_struct;
    if (!test_contains_large_struct(cls)) {
        return 31;
    }

    union gp_and_xmm gax2 = gax;
    gax2.d_arr[0] = -2.0;
    gax2.d_arr[1] = -1.0;
    union contains_union_array cua = {
        {gax, gax2}
    };
    if (!test_contains_union_array(cua)) {
        return 32;
    }

    return 0; // success
}