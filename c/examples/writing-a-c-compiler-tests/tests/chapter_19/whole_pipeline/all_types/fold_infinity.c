/* Test constant folding with infinity.
 * We couldn't test this in the constant folding stage because it requires
 * copy propagation.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-range"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

double copysign(double x, double y);  // from standard library

// helper functions
int is_positive_zero(double d) {
    if (d != 0.0) {
        // it's non-zero
        return 0;
    }
    // make sure the sign is positive
    return (copysign(5., d) == 5.0);
}

int is_negative_zero(double d) {
    if (d != 0.0) {
        // it's non-zero
        return 0;
    }
    // make sure the sign is negative
    return (copysign(5., d) == -5.);
}

int target_infinity_equal(void) {
    // 1.0 / 0.0 evaluates to infinity
    // 11e330 rounds to infinity
    // infinity compares equal to itself
    return 1.0 / 0.0 == 11e330;
}

int target_infinity_gt(void) {
    // 1.0 / 0.0 evaluates to infinity
    // infinity compares greater than all finite value
    return 1.0 / 0.0 > 1.79E308;
}

int target_neg_infinity_lt(void) {
    // -1.0 / 0.0 evaluates to negative infinity
    // which compares less than all finite values
    return -1.0 / 0.0 < -1.79E308;
}

// adding any finite number to infinity results in infinity
double target_add_infinity(void) {
    return (1.0 / 0.0) + 1000e10;
}

// subtracting a finite number from infinity results in infinity
double target_sub_infinity(void) {
    return (1.0 / 0.0) - 1000e10;
}

// any finite number times infinity is positive or negative infinity
// with the sign following the usual rules for multiplication
double target_mult_infinity(void) {
    return (1.0 / 0.0) * 25.;  // infinity
}

double target_mult_neg_infinity(void) {
    return (-1.0 / 0.0) * 25.;  // negative infinity
}

double target_mult_neg_infinity_by_neg_number(void) {
    return (-1.0 / 0.0) * -25.;  // positive infinity
}

// infinity divided by any finite number is infinity
// with the sign following the usual rules for divison
double target_div_infinity_by_num(void) {
    return (1. / 0.) / 5.;
}

// negating infinity produces negative infinity
double target_negate_inf(void) {
    double infin = 1.0 / 0.0;
    return -infin;
}

// if the true result of an operation is bigger than the largest representable
// double, it results in infinity
// note that these tests call this "overflow" but technically it's not - it's
// well-defined
double target_mult_overflow(void) {
    return 2e300 * 10e20;  // infinity
}

double target_div_overflow(void) {
    return 1e308 / -10e-20;  // negative infinity
}

double target_add_overflow(void) {
    return 1.79e308 + 1e308;  // infinity
}

double target_sub_overflow(void) {
    return -1e308 - 1e308;  // negative infinity
}

// 0 divided by infinity is zero
// signs follow the usual rules for division
double target_zero_div_by_inf(void) {
    return 0. / (3.0 / 0.);  // zero
}

double target_zero_div_by_neg_inf(void) {
    return 0. / (3.0 / -0.);  // negative zero
}

double target_negative_zero_div_by_inf(void) {
    return -0. / (0.005 / 0.);  // negative zero
}

// infinity evaluates to true in conditional
int target_infinity_is_true(void) {
    double infin = 2345.0 / 0.0;
    if (infin) {
        return 1;  // success
    }
    return 0;  // failure
}

double zero = 0.;

int main(void) {
    // calculate infinity here with an expression we can't constant fold,
    // so the value we validate against is right even if constant folding is
    // incorrect
    double infinity = 1. / zero;

    if (!target_infinity_equal()) {
        return 1;  // fail
    }
    if (!target_infinity_gt()) {
        return 2;  // fail
    }
    if (!target_neg_infinity_lt()) {
        return 3;  // fail
    }

    if (target_add_infinity() != infinity) {
        return 4;  // fail
    }

    if (target_sub_infinity() != infinity) {
        return 5;  // fail
    }
    if (target_mult_infinity() != infinity) {
        return 6;  // fail
    }

    if (target_mult_neg_infinity() != -infinity) {
        return 7;  // fail
    }
    if (target_mult_neg_infinity_by_neg_number() != infinity) {
        return 8;  // fail
    }
    if (target_div_infinity_by_num() != infinity) {
        return 9;  // fail
    }
    if (target_negate_inf() != -infinity) {
        return 10;  // fail
    }
    if (target_mult_overflow() != infinity) {
        return 11;  // fail
    }
    if (target_div_overflow() != -infinity) {
        return 12;  // fail
    }
    if (target_add_overflow() != infinity) {
        return 13;  // fail
    }
    if (target_sub_overflow() != -infinity) {
        return 14;  // fail
    }
    double d = target_zero_div_by_inf();
    if (!is_positive_zero(d)) {
        return 15;  // fail
    }
    d = target_zero_div_by_neg_inf();
    if (!is_negative_zero(d)) {
        return 16;  // fail
    }
    d = target_negative_zero_div_by_inf();
    if (!is_negative_zero(d)) {
        return 17;  // fail
    }
    if (target_infinity_is_true() != 1) {
        return 18;  // fail
    }
    return 0;  // success
}