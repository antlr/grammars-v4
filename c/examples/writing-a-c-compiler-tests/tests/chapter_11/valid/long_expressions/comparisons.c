/* Test comparisons between longs, making sure to exercise all rewrite rules for cmp */

long l;
long l2;

/* Comparisons where both operands are constants */
int compare_constants(void) {
    /* Note that if we considered only the lower 32 bits of
     * each number (or cast them to ints), 255 would be larger,
     * because 8589934593l == 2^33 + 1.
     * This exercises the rewrite rule for cmp with two constant operands
     */
    return 8589934593l > 255l;
}

int compare_constants_2(void) {
    /* This exercises the rewrite rule for cmp where src is a large constant
     * and dst is a constant, because 8589934593 can't fit in an int.
     */
    return 255l < 8589934593l;
}

int l_geq_2_60(void) {
    /* This exercises the rewrite rule for cmp where src is a large constant
     * and dst is a variable.
     * 1152921504606846976l == 2^60
     */
    return (l >= 1152921504606846976l);
}

int uint_max_leq_l(void) {
    /* The first operand to cmp is a variable and second is a constant (UINT_MAX as a long). */
    return (4294967295l <= l);
}

int l_eq_l2(void) {
    /* Exercise rewrite rule for cmp where both operands are in memory */
    return (l == l2);
}

int main(void) {

    if (!compare_constants()) {
        return 1;
    }

    if (!compare_constants_2()) {
        return 2;
    }

    l = -9223372036854775807l; // LONG_MIN + 1
    if (l_geq_2_60()) {
        return 3;
    }
    if (uint_max_leq_l()) {
        return 4;
    }
    l = 1152921504606846976l; // 2^60
    if (!l_geq_2_60()) {
        return 5;
    }
    if (!uint_max_leq_l()) {
        return 6;
    }
    l2 = l;
    if (!l_eq_l2()) {
        return 7;
    }
    return 0;
}
