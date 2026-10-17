/* Test constant folding of all operations on unsigned ints;
 * make sure they wrap around correctly
 * and that we evaluate them with unsigned division/comparison functions.
 */
unsigned int target_add(void) {
    // result exceeds UINT_MAX and wraps around past 0
    return 4294967295U + 10u;
}

unsigned int target_sub(void) {
    // result is less then 0 and wraps back round past UINT_MAX
    return 10u - 12u;
}

unsigned int target_mult(void) {
    // wraps back around to 2147483648u
    return 2147483648u * 3u;
}

unsigned int target_div(void) {
    // result would be different if we interpreted values as signed
    return 4294967286u / 10u;
}

unsigned int target_rem(void) {
    // result would be different if we interpreted values as signed
    return 4294967286u % 10u;
}

unsigned int target_complement(void) {
    return ~1u;
}

unsigned int target_neg(void) {
    return -10u;
}

int target_not(void) {
    return !65536u;  // 2^16
}

int target_eq(void) {
    return 100u == 100u;
}

int target_neq(void) {
    // these have identical binary representations except for the most
    // significant bit
    return 2147483649u != 1u;
}

int target_gt(void) {
    // make sure we're using unsigned comparisons;
    // if we interpret these as signed integers,
    // we'll think 2147483649u is negative and return 0
    return 2147483649u > 1000u;
}

int target_ge(void) {
    return 4000000000u >= 3999999999u;
}

int target_lt(void) {
    // as with target_gt, make sure we don't interpret 2147483649u
    // as a negative signed integer
    return 2147483649u < 1000u;
}

int target_le(void) {
    return 4000000000u <= 3999999999u;
}

int main(void) {
    // binary arithmetic
    if (target_add() != 9u) {
        return 1;
    }
    if (target_sub() != 4294967294U) {
        return 2;
    }
    if (target_mult() != 2147483648u) {
        return 3;
    }
    if (target_div() != 429496728u) {
        return 4;
    }
    if (target_rem() != 6u) {
        return 5;
    }

    // unary operators
    if (target_complement() != 4294967294U) {
        return 6;
    }

    if (target_neg() + 10 != 0) {
        return 7;
    }

    if (target_not() != 0) {
        return 8;
    }

    // comparisons
    if (!target_eq()) {
        return 9;
    }
    if (!target_neq()) {
        return 10;
    }
    if (!target_gt()) {
        return 11;
    }
    if (!target_ge()) {
        return 12;
    }
    if (target_lt()) {
        return 13;
    }
    if (target_le()) {
        return 14;
    }

    return 0;
}
