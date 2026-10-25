#include "union_lib.h"

int test_one_double(union one_double u) {
    return (u.d1 == -2.345e6 && u.d2 == -2.345e6);
}
int test_has_union_with_double(struct has_union_with_double s) {
    return (s.member.d1 == 9887.54321e44 && s.member.d2 == 9887.54321e44);
}

int test_has_struct_with_double(union has_struct_with_double u) {
    return (u.s.member.d1 == 9887.54321e44
        && u.arr[0] == 9887.54321e44 && u.s.member.d2 == 9887.54321e44);
}
int test_one_int(union one_int u) {
    return (u.d == -80. && u.c == 0);
}
int test_one_int_nested(union one_int_nested u) {
    return u.oi.d == 44e55 && u.oi.c == 109 && u.od.d1 == 44e55
        && u.od.d2 == 44e55;
}
int test_char_int_mixed(union char_int_mixed u) {
    return (strcmp(u.arr, "WXYZ") == 0 && u.ui == 1515804759);
}

int test_has_union(struct has_union s) {
    return (s.i == 4294954951u && s.u.c == -60);
}
int test_has_struct_with_ints(union has_struct_with_ints u) {
    return (u.s.i == 4294954951u && u.s.u.c == -60);
}

int test_two_doubles(union two_doubles u) {
    return (u.arr[0] == 10.0 && u.arr[1] == 11.0 && u.single == 10.0);
}

int test_has_xmm_union(union has_xmm_union u) {
    return u.u.d1 == 10.0 && u.u.d2 == 10.0 && u.u2.single == 10.0
        && u.u2.arr[0] == 10.0 && u.u2.arr[1] == 11.0;
}
int test_dbl_struct(struct dbl_struct s) {
    return s.member1.d1 == -2.345e6 && s.member1.d2 == -2.345e6
        && s.member2 == 123.45;
}

int test_has_dbl_struct(union has_dbl_struct u) {
    return u.member1.member1.d1 == -2.345e6 && u.member1.member1.d2 == -2.345e6
        && u.member1.member2 == 123.45;
}

int test_char_arr(union char_arr u) {
    return (strcmp(u.arr, "Chars!") == 0 && u.i == 1918986307);
}

int test_two_arrs(union two_arrs u) {
    return (u.dbl_arr[0] == 13e4 && u.dbl_arr[1] == 14.5
        && u.long_arr[0] == 4683669945186254848 && u.long_arr[1] == 4624352392379367424);
}

int test_two_eightbyte_has_struct(union two_eightbyte_has_struct u) {
    return (u.arr[0] == 100 && u.arr[1] == 200 && u.arr[2] == 300
        && u.member1.member1.d1 == 4.24399158242461027606e-312);
}
int test_two_structs(union two_structs u) {
    return (u.member1.c == 'x' && u.member1.d == 55.5e5 && u.member2.i == 0);
}
int test_has_nine_byte_struct(union has_nine_byte_struct u) {
    if (u.l != -71777214294589696l || u.c != 0) {
        return 0;
    }
    if (u.s.i != -16711936) {
        return 0;
    }
    for (int i = 0; i < 5; i = i + 1) {
        int expected = i % 2 ? -1 : 0;
        if (u.s.arr[i] != expected) {
            return 0;
        }
    }

    return 1; // success
}
int test_has_uneven_union(struct has_uneven_union s) {
    return s.i == -2147483647 && strcmp(s.u.arr, "!@#$") == 0 && s.u.uc == 33;
}

int test_has_other_unions(union has_other_unions u) {
    if (u.n.l != -71777214294589696l) {
        return 0;
    }
    for (int i = 0; i < 5; i = i + 1) {
        int expected = i % 2 ? -1 : 0;
        if (u.n.s.arr[i] != expected) {
            return 0;
        }
    }

    return 1; // success
}
int test_union_array(union union_array u) {
    return (u.u_arr->d == -20. && u.u_arr[1].d == -30.);
}

int test_uneven_union_array(union uneven_union_array u) {
    return (strcmp(u.u_arr[0].arr, "QWER") == 0 && strcmp(u.u_arr[1].arr, "TYUI") == 0);
}

int test_has_small_struct_array(union has_small_struct_array u) {
    return strcmp(u.arr[0].arr, "AS") == 0 && u.arr[0].sc == 10
        && strcmp(u.arr[1].arr, "DF") == 0 && u.arr[1].sc == 11
        && strcmp(u.arr[2].arr, "GH") == 0 && u.arr[2].sc == 12;
}
int test_gp_and_xmm(union gp_and_xmm u) {
    return u.d_arr[0] == 11. && u.d_arr[1] == 12.;
}

int test_scalar_and_struct(union scalar_and_struct u) {
    return u.cfe.c == -5 && u.cfe.d == -88.8;
}

int test_has_two_unions(struct has_two_unions s) {
    if (strcmp(s.member1.arr, "WXYZ")) {
        return 0;
    }

    if (s.member2.d1 != -2.345e6) {
        return 0;
    }

    return 1;

}

int test_small_struct_arr_and_dbl(union small_struct_arr_and_dbl u) {
    return (u.d.arr[0] == -22. && u.d.arr[1] == -32.);
}

int test_xmm_and_gp(union xmm_and_gp u) {
    return (u.ise.d == -8. && u.ise.i == -8);
}

int test_xmm_and_gp_nested(union xmm_and_gp_nested u) {
    return (u.member1.ise.d == -8. && u.member1.ise.i == -8);
}
int test_lotsa_doubles(union lotsa_doubles u) {
    return u.arr[0] == 99. && u.arr[1] == 98. && u.arr[2] == 97;
}

int test_lotsa_chars(union lotsa_chars u) {
    return !strcmp(u.more_chars, "asflakjsdflkjs");
}

int test_contains_large_struct(union contains_large_struct u) {
    return u.l.i == 100 && u.l.d == 100. && !strcmp(u.l.arr, "A struct!");
}
int test_contains_union_array(union contains_union_array u) {
    union gp_and_xmm a = u.arr[0];
    union gp_and_xmm b = u.arr[1];

    if (a.d_arr[0] != 11. || a.d_arr[1] != 12.) {
        return 0;
    }
    if (b.d_arr[1] != -1 || b.c != 0) {
        return 0;
    }
    return 1;
}