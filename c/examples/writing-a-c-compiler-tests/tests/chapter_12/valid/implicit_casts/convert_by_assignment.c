/* Test that we correctly perform conversions "as if by assignment", including:
 * - function arguments
 * - return statements
 * - actual assignment expressions
 * - initializers for automatic variables
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

int check_int(int converted, int expected) {
    return (converted == expected);
}

int check_long(long converted, long expected) {
    return (converted == expected);
}

int check_ulong(unsigned long converted, unsigned long expected) {
    return (converted == expected);
}

long return_extended_uint(unsigned int u) {
    return u;
}

unsigned long return_extended_int(int i) {
    return i;
}

int return_truncated_ulong(unsigned long ul) {
    return ul;
}

int extend_on_assignment(unsigned int ui, long expected) {
    long result = ui; // implicit conversion causes zero-extension
    return result == expected;
}

int main(void) {
    // function arguments

    /* truncate 2^63 + 5 to 5 */
    if (!check_int(9223372036854775813ul, 5)) {
        return 1;
    }

    /* zero-extend 2^31+10, preserve its value */
    if (!check_long(2147483658u, 2147483658l)) {
        return 2;
    }

    /* sign-extend -1 to ULONG_MAX */
    if (!check_ulong(-1, 18446744073709551615UL)) {
        return 3;
    }

    // return values

    /* zero-extend 2^31+10, preserve its value */
    if (return_extended_uint(2147483658u) != 2147483658l) {
        return 4;
    }

    /* sign-extend -1 to ULONG_MAX */
    if (return_extended_int(-1) != 18446744073709551615UL) {
        return 5;
    }

    /* truncate 2^50 + 2^31 + 100 to int, -2^31 + 100
     * then sign-extend, preserving its value
     */
    long l = return_truncated_ulong(1125902054326372ul);
    if (l != -2147483548l) {
        return 6;
    }

    // assignment expressions
    if (!extend_on_assignment(2147483658u, 2147483658l)){
        return 7;
    }

    // local initializers
    int i = 4294967196u; // unsigned int 2^32 - 100, will be converted to -100
    if (i != -100) {
        return 8;
    }


    return 0;
}