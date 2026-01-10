#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wsign-compare"
#endif
/* Test that we correctly find the common type of different integers */

int int_gt_uint(int i, unsigned int u) {
    // common type is unsigned int
    return i > u;
}

int int_gt_ulong(int i, unsigned long ul) {
    // common type is unsigned long
    return i > ul;
}

int uint_gt_long(unsigned int u, long l) {
    // common type is long
    return u > l;
}

int uint_lt_ulong(unsigned int u, unsigned long ul) {
    // common type is unsigned long
    return u < ul;
}

int long_gt_ulong(long l, unsigned long ul) {
    // common type is unsigned long
    return l > ul;
}

int ternary_int_uint(int flag, int i, unsigned int ui) {
    /* flag = 1
     * i = -1
     * ui = 10u
     * The common type of i and ui is unsigned int
     * (we don't consider the type of cond when we
     * determine the common type).
     * We therefore convert i to an unsigned int, 2^32 - 1,
     * which we then convert to a signed long.
     * Therefore, result will be positive. If we didn't
     * convert i to an unsigned int, result would be negative.
     */
    long result = flag ? i : ui;
    return (result == 4294967295l);

}

int main(void) {

    // converting -100 from int to unsigned int gives us 2^32 - 100,
    // so -100 > 100u
    if (!int_gt_uint(-100, 100u)) {
        return 1;
    }

    // converting -1 to unsigned long gives us 2^64-1, i.e. INT_MAX
    // 18446744073709551606 is 2^64 - 10
    if (!(int_gt_ulong(-1, 18446744073709551606ul))) {
        return 2;
    }

    // converting 100u to a signed long won't change its value
    // Note that if we converted -100 to an unsigned int it would be
    // greater than 100
    if (!uint_gt_long(100u, -100l)) {
        return 3;
    }

    // converting an unsigned int to an unsigned long won't change its value
    // if we converted 34359738368ul (2^35) to an unsigned int its value would be 0
    // Note: 1073741824u is 2^30
    if (!uint_lt_ulong(1073741824u, 34359738368ul)) {
        return 4;
    }

    // converting -1 from long to unsigned long gives us 2^64 -1, so -1l > 1000ul
    if (!long_gt_ulong(-1l, 1000ul)) {
        return 5;
    }

    // make sure we convert the two branches of a ternary expression to the common type
    if (!ternary_int_uint(1, -1, 1u)) {
        return 6;
    }

    return 0;

}