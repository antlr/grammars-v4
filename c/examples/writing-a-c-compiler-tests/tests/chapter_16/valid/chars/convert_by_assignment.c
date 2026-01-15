/* Test implicit conversions to and from character types
 * as if by assignment.
 * This test includes integer promotions, but isn't
 * explicitly focused on them.
 * */

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// helper functions
int check_int(int converted, int expected) {
    return (converted == expected);
}

int check_uint(unsigned int converted, unsigned int expected) {
    return (converted == expected);
}

int check_long(long converted, long expected) {
    return (converted == expected);
}

int check_ulong(unsigned long converted, unsigned long expected) {
    return (converted == expected);
}

int check_double(double converted, double expected) {
    return (converted == expected);
}

int check_char(char converted, char expected) {
    return (converted == expected);
}

int check_uchar(unsigned char converted, unsigned char expected) {
    return (converted == expected);
}

int check_char_on_stack(signed char expected, int dummy1, int dummy2,
                        int dummy3, int dummy4, int dummy5, int dummy6,
                        signed char converted) {
    return converted == expected;
}

// implicitly convert a return value from a character type to another type
int return_extended_uchar(unsigned char c) {
    return c;
}

unsigned long return_extended_schar(signed char sc) {
    return sc;
}

// implicitly truncate a return value from int to unsigned char
unsigned char return_truncated_long(long l) {
    return l;
}

int main(void) {
    /* Function arguments */
    signed char sc = -10;

    // converting sc to a signed type preserves its value
    if (!check_long(sc, -10l)) {
        return 1;
    }

    // converting to an unsigned type works the same as for other signed ints
    if (!check_uint(sc, 4294967286u)) {  // UINT_MAX - 10
        return 2;
    }

    // converting to double preserves its value
    // (double can represent all characters exactly)
    if (!check_double(sc, -10.0)) {
        return 3;
    }

    // to convert to unsigned, add UCHAR_MAX until its in ragen
    unsigned char uc = 246;
    if (!check_uchar(sc, uc)) {
        return 4;
    }

    // convert parameters from other types to char
    char c = -10;
    if (!check_char(-10, c)) {
        return 5;
    }

    if (!check_char(4294967286u, c)) {
        return 6;
    }
    if (!check_char(-10.0, c)) {
        return 7;
    }

    if (!check_char_on_stack(c, 0, 0, 0, 0, 0, 0, -10.0)) {
        return 8;
    }

    // can also implicitly convert argument from uchar to other types
    // (this never changes its value unless we're converting to char/signed
    // char)

    // uc is still 246
    if (!check_int(uc, 246)) {
        return 9;
    }

    if (!check_ulong(uc, 246ul)) {
        return 10;
    }

    char expected_char = -10;
    if (!check_char(uc, expected_char)) {
        return 11;
    }

    // and from other types to uchar

    if (!check_uchar(18446744073709551606ul, uc)) {
        return 12;
    }

    /* Return values */
    if (return_extended_uchar(uc) != 246) {
        return 13;
    }

    if (return_extended_schar(sc) != 18446744073709551606ul) {  // UINT_MAX - 10
        return 14;
    }

    if (return_truncated_long(5369233654l) != uc) {  // 2^19 + 2^30 + 2^32 - 10
        return 15;
    }

    /* Assignment */

    // assign to signed char
    // make sure that the char gets the right value and that neighboring values
    // aren't overwritten
    char array[3] = {0, 0, 0};

    // update middle element, make sure other elements are unchanged
    array[1] = 128;  // int
    if (array[0] || array[2] || array[1] != -128) {
        return 16;
    }

    array[1] = 9224497936761618562ul;  // unsigned long - truncated to -126
    if (array[0] || array[2] || array[1] != -126) {
        return 17;
    }

    array[1] = -2.6;
    if (array[0] || array[2] || array[1] != -2) {
        return 18;
    }

    // assign to unsigned char - same idea as for signed char
    unsigned char uchar_array[3] = {0, 0, 0};

    uchar_array[1] = 17592186044416l;  // long
    if (uchar_array[0] || uchar_array[2] || uchar_array[1] != 0) {
        return 19;
    }

    uchar_array[1] = 2147483898u;  // unsigned int
    if (uchar_array[0] || uchar_array[2] || uchar_array[1] != 250) {
        return 20;
    }

    // assign char and uchar to other types - make sure value is sign- or
    // zero-extended to whole value
    unsigned int ui = 4294967295U;
    static unsigned char
        uc_static;  // this is static so it can't be copy propped in Part III
    ui = uc_static;

    if (ui) {
        return 21;
    }

    signed long l = -1;
    static signed s_static =
        0;  // this is static so it can't be copy propped in Part III
    l = s_static;
    if (l) {
        return 22;
    }

    return 0;
}