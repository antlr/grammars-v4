/* Test constant folding of all operations on doubles and make sure they're
 * correctly rounded.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-range"
#pragma clang diagnostic ignored "-Wliteral-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

double target_add(void) {
    // Because 1.2345e60 is so large, adding one to it doesn't change its value
    return 1.2345e60 + 1.;
}

double target_sub(void) {
    // make sure we properly calculate the difference between two very close
    // subnormal numbers
    return 5.85543871245623688067e-311 - 5.85543871245574281503e-311;
}

double target_mult(void) {
    return 2.1 * 3.0;
}

double target_div(void) {
    return 1100.5 / 5000.;
}

double target_div_underflow(void) {
    // this result should underflow to zero
    return 0.5e-100 / 2e307;
}

double target_neg(void) {
    return -.000000275;
}

int target_not(void) {
    return !1e30;
}

int target_eq(void) {
    // these decimal constants should be rounded to the same floating-point
    // value, so this will return 1
    return 0.1 == 0.10000000000000001;
}

int target_neq(void) {
    // these should compare unequal; 5e-324 will be rounded to the subnormal
    // number just above zero
    return 5e-324 != 0.0;
}

int target_gt(void) {
    return 1e308 > 1e307;
}

int target_ge(void) {
    return 3.1 >= 3.1;
}

int target_lt(void) {
    // these decimal constants should be rounded to the same floating-point
    // value, so this will return 0
    return 0.1 < 0.10000000000000001;
}

int target_le(void) {
    return 0.5 <= 0.;
}

double target_negate_zero(void) {
    // make sure this gives us negative zero and not zero
    return -0.0;
}

double target_infinity(void) {
    // this will result in infinity
    return 1e308 * 2.;
}

int target_compare_infininty(void) {
    // infinity == infinity
    return 10e308 == 12e308;
}

int main(void) {
    // arithmetic
    if (target_add() != 1.2345e60) {
        return 1;
    }
    if (target_sub() != 5e-324) {
        return 2;
    }
    if (target_mult() != 6.300000000000001) {
        return 3;
    }

    if (target_div() != 0.2201) {
        return 4;
    }
    if (target_div_underflow() != 0.0) {
        return 5;
    }
    // add to result and compare to 0, instead of comparing result to
    // -.000000275, so we don't constant-fold -.000000275 here
    if (target_neg() + .000000275 != 0.0) {
        return 6;
    }

    // logical ops and comparisons
    if (target_not() != 0) {
        return 7;
    }
    if (!target_eq()) {
        return 8;
    }
    if (!target_neq()) {
        return 9;
    }
    if (!target_gt()) {
        return 10;
    }
    if (!target_ge()) {
        return 11;
    }
    if (target_lt()) {
        return 12;
    }
    if (target_le()) {
        return 13;
    }

    // infinity
    if (target_infinity() != 10e308) {
        return 14;
    }
    if (!target_compare_infininty()) {
        return 15;
    }

    // check that we got -0.0, not 0.0;
    // 1/0 is infinity, 1/-0 is negative infinity
    if (!((1 / target_negate_zero()) < 0.0)) {
        return 16;
    }

    return 0;
}
