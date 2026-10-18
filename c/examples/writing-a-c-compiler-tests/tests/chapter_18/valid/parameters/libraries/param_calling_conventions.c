/* Test that we can pass a mix of struct and non-struct arguments according to
 * the ABI */

#include "param_calling_conventions.h"

// all arguments fit in registers
int pass_small_structs(struct two_xmm two_xmm_struct, struct one_int int_struct,
                       struct one_xmm xmm_struct,
                       struct xmm_and_int mixed_struct,
                       struct twelve_bytes int_struct_2,
                       struct one_int_exactly another_int_struct) {
    if (two_xmm_struct.d[0] != 55.5 || two_xmm_struct.d[1] != 44.4)
        return 0;

    if (int_struct.c != 'c' || int_struct.i != 54320)
        return 0;
    if (xmm_struct.d != 5.125)
        return 0;
    if (strcmp(mixed_struct.c, "hi") || mixed_struct.dbl.d != 1.234)
        return 0;
    if (strcmp(int_struct_2.arr, "string!") || int_struct_2.i != 123)
        return 0;

    if (another_int_struct.l != 567890)
        return 0;

    return 1;  // success
}

// based on example in Listing 18-45
int a_bunch_of_arguments(int i0, int i1, int i2, int i3, int i4,
                         struct two_longs param, int i5) {
    if (i0 != 0 || i1 != 1 || i2 != 2 || i3 != 3 || i4 != 4 || i5 != 5) {
        return 0;
    }

    if (param.a != 1234567l || param.b != 89101112l) {
        return 0;
    }

    return 1;  // success
}

// use remaining structure types, mix with scalars
int structs_and_scalars(long l, double d, struct odd_size os, struct memory mem,
                        struct one_xmm xmm_struct) {
    if (l != 10)
        return 0;
    if (d != 10.0)
        return 0;
    if (strcmp(os.arr, "lmno"))
        return 0;
    if (strcmp(mem.c, "rs") || mem.d != 15.75 || mem.i != 3333 || mem.l != 4444)
        return 0;
    if (xmm_struct.d != 5.125)
        return 0;

    return 1;  // success
}

// pass fourth_struct in memory b/c we're out of XMM registers
int struct_in_mem(double a, double b, double c, struct xmm_and_int first_struct,
                  double d, struct two_xmm second_struct, long l,
                  struct int_and_xmm third_struct,
                  struct one_xmm fourth_struct) {
    if (a != 10.0 || b != 11.125 || c != 12.0)
        return 0;
    if (strcmp(first_struct.c, "hi") || first_struct.dbl.d != 1.234)
        return 0;
    if (d != 13.0)
        return 0;
    if (second_struct.d[0] != 55.5 || second_struct.d[1] != 44.4)
        return 0;
    if (l)
        return 0;
    if (third_struct.c != 'p' || third_struct.d != 4.56)
        return 0;
    if (fourth_struct.d != 5.125)
        return 0;

    return 1;  // success
}

// pass two_ints_nested in memory - we have one general-purpose reg left for
// parameter passing but it requires two
int pass_borderline_struct_in_memory(struct two_ints t_i, int c,
                                     struct int_and_xmm i_x, void *ptr,
                                     struct two_ints_nested t_i_n, double d) {
    if (t_i.c != '_' || t_i.arr[0] != 5 || t_i.arr[1] != 6 || t_i.arr[2] != 7)
        return 0;
    if (c != '!')
        return 0;
    if (i_x.c != 'p' || i_x.d != 4.56)
        return 0;

    if (ptr)
        return 0;

    if (t_i_n.a.c != 'c' || t_i_n.a.i != 54320)
        return 0;
    if (t_i_n.b.c != 'c' || t_i_n.b.i != 54320)
        return 0;
    if (d != 7.8)
        return 0;
    return 1;  // success
}

// pass a struct in memory that isn't neatly divisible by 8
int pass_uneven_struct_in_mem(struct twelve_bytes struct1, long a, long b,
                              struct twelve_bytes struct2, struct odd_size os,
                              struct memory m) {
    if (struct1.i != -1) {
        return 0;
    }
    if (struct1.arr[0] != 127 || struct1.arr[1] != 126 ||
        struct1.arr[2] != 125) {
        return 0;
    }
    if (a != 9223372036854775805l || b != 9223372036854775800l) {
        return 0;
    }
    if (struct2.i != -5) {
        return 0;
    }
    if (struct2.arr[0] != 100 || struct2.arr[1] != 101 ||
        struct2.arr[2] != 102) {
        return 0;
    }
    for (int i = 0; i < 5; i = i + 1) {
        if (os.arr[i] != 100 - i) {
            return 0;
        }
    }
    if (m.d != 5.345) {
        return 0;
    }
    if (m.c[0] != -1 || m.c[1] != -2 || m.c[2] != -3) {
        return 0;
    }
    if (m.l != 4294967300l) {
        return 0;
    }
    if (m.i != 10000) {
        return 0;
    }
    return 1;  // success
}

int pass_later_structs_in_regs(struct memory m, struct twelve_bytes struct1,
                               struct one_xmm struct2) {
    if (m.d != 5.345) {
        return 0;
    }

    if (m.c[0] != -1 || m.c[1] != -2 || m.c[2] != -3) {
        return 0;
    }

    if (m.l != 4294967300l) {
        return 0;
    }

    if (m.i != 10000) {
        return 0;
    }

    if (struct1.i != -1) {
        return 0;
    }
    if (struct1.arr[0] != 127 || struct1.arr[1] != 126 ||
        struct1.arr[2] != 125) {
        return 0;
    }

    if (struct2.d != 5.125) {
        return 0;
    }
    return 1;  // success
}