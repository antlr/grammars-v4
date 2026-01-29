/* Test constant folding with negative zero.
 * We couldn't test this in the constant folding stage because it requires
 * copy propagation.
 * See https://en.wikipedia.org/wiki/Signed_zero#Properties_and_handling for
 * a quick reference on arithmetic operations with -0.0
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
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

/* Comparisons */

// 0.0 and -0.0 should compare equal
int target_negative_zero_eq(void) {
    return 0.0 == -0.0;  // true
}

int target_negative_zero_neq(void) {
    return 0.0 != -0.0;  // false
}

int target_negative_zero_lt(void) {
    return -0.0 < 0.0;  // false - they're equal, so -0. is not less than 0.
}

int target_negative_zero_gt(void) {
    return -0.0 > 0.0;  // false - they're equal, so 0. is not greater than 0.
}

int target_negative_zero_ge(void) {
    return -0.0 >= 0.0;  // true b/c operands are equal
}

int target_negative_zero_le(void) {
    return -0.0 <= 0.0;  // true b/c operands are equal
}

// make sure -0.0 counts as 0 in conditionals/booleans
int target_neg_zero_branch(void) {
    if (-0.0) {
        return 1;
    }
    return 0;
}

int target_not_neg_zero(void) {
    return !-0.0;  // 1
}

int target_neg_zero_and(void) {
    return -0.0 && 1;
}

/* Addition */
double target_add_negative_zero_to_self(void) {
    return -0. + -0.;  // negative (special case)
}

double target_add_positive_to_negative_zero(void) {
    // posiive (-0.0 + 0.0 ==> 0.0 -- 0.0, and x - x is always +0.0)
    return -0. + 0.;
}

double target_add_negative_to_positive_zero(void) {
    return 0. + -0.;  // positive; equivalent to previous case
}

double target_add_negative_nonzero_to_negative_zero(void) {
    // adding any number to negative zero results in the same number
    return -5.0 + -0.;
}

double target_add_positive_nonzero_to_negative_zero(void) {
    // adding any number to negative zero results in the same number
    return -0. + 10.;
}

/* Subtraction */
double target_subtract_neg_zero_from_self(void) {
    return -0. - -0.;  // positive zero; equivalent to 0. - 0.
}

double target_subtract_pos_zero_from_neg_zero(void) {
    return -0. - 0.;  // negative zero; equivalent to -0. + -0.
}

double target_subtract_neg_zero_from_pos_zero(void) {
    return 0. - -0.;  // positive zero; equivalent to 0. + 0.
}

double target_subtract_pos_nonzero_from_neg_zero(void) {
    return -0. - 10.;  // -10.
}

/* Multiplication */
double target_negative_zero_mult(void) {
    return -0.0 * 15.4e10;  // negative zero
}

double target_negative_zero_mult_negative(void) {
    return -100. * -0.;  // positive zero
}

double target_negative_zero_squared(void) {
    return -0. * -0.;  // positive zero
}

double target_neg_zero_mult_zero(void) {
    return 0. * -0.;  // negative zero
}

double target_mult_underflow(void) {
    return -5.85543871245623688067e-320 *
           0.5e-5;  // underflows to negative zero
}

/* division */

double target_div_neg_zero_by_pos_nonzero(void) {
    return -0. / 10.;  // negative zero
}

double target_div_pos_zero_by_neg_nonzero(void) {
    return 0. / -10.;  // negative zero
}

double target_div_neg_zero_by_neg_nonzero(void) {
    return -0. / -5.;  // positive zero
}

double target_div_negative_underflow(void) {
    // this result should underflow to negative zero
    return 0.5e-100 / -2e307;
}

double target_div_pos_non_zero_by_neg_zero(void) {
    return 10. / -0.0;  // negative infinity
}

double target_div_neg_nonzero_by_zero(void) {
    return -10. / 0.;  // negative infinity
}

double target_div_neg_nonzero_by_neg_zero(void) {
    return -100. / -0.;  // positive infinity
}

/* negation */
double target_negate_neg_zero(void) {
    return -(-0.0);  // positive zero
}

// we can calculate zero and negate it (i.e. we're not just treating the
// source-level expression -0.0 as a special case)
double target_negate_calculated_zero(void) {
    return -(50. - 50.);
}

int main(void) {
    if (target_negative_zero_eq() != 1) {
        return 1;  // fail
    }
    if (target_negative_zero_neq() != 0) {
        return 2;  // fail
    }
    if (target_negative_zero_lt() != 0) {
        return 3;  // fail
    }
    if (target_negative_zero_gt() != 0) {
        return 4;  // fail
    }
    if (target_negative_zero_ge() != 1) {
        return 5;  // fail
    }
    if (target_negative_zero_le() != 1) {
        return 6;  // fail
    }
    if (target_neg_zero_branch()) {
        return 7;  // fail
    }
    if (target_not_neg_zero() != 1) {
        return 8;  // fail
    }
    if (target_neg_zero_and() != 0) {
        return 9;  // fail
    }

    double d;
    d = target_add_negative_zero_to_self();
    if (!is_negative_zero(d)) {
        return 10;  // fail
    }
    d = target_add_positive_to_negative_zero();
    if (!is_positive_zero(d)) {
        return 11;  // fail
    }
    d = target_add_negative_to_positive_zero();
    if (!is_positive_zero(d)) {
        return 12;  // fail
    }
    d = target_add_negative_nonzero_to_negative_zero();
    if (d != -5.) {
        return 13;  // fail
    }
    d = target_add_positive_nonzero_to_negative_zero();
    if (d != 10.) {
        return 14;  // fail
    }

    d = target_subtract_neg_zero_from_self();
    if (!is_positive_zero(d)) {
        return 15;  // fail
    }
    d = target_subtract_pos_zero_from_neg_zero();
    if (!is_negative_zero(d)) {
        return 16;  // fail
    }
    d = target_subtract_neg_zero_from_pos_zero();
    if (!is_positive_zero(d)) {
        return 17;  // fail
    }
    d = target_subtract_pos_nonzero_from_neg_zero();
    if (d != -10.) {
        return 18;  // fail
    }

    d = target_negative_zero_mult();
    if (!is_negative_zero(d)) {
        return 19;  // fail
    }
    d = target_negative_zero_mult_negative();
    if (!is_positive_zero(d)) {
        return 20;  // fail
    }
    d = target_negative_zero_squared();
    if (!is_positive_zero(d)) {
        return 21;  // fail
    }
    d = target_neg_zero_mult_zero();
    if (!is_negative_zero(d)) {
        return 22;  // fail
    }
    d = target_mult_underflow();
    if (!is_negative_zero(d)) {
        return 23;  // fail
    }

    d = target_div_neg_zero_by_pos_nonzero();
    if (!is_negative_zero(d)) {
        return 24;  // fail
    }
    d = target_div_pos_zero_by_neg_nonzero();
    if (!is_negative_zero(d)) {
        return 25;  // fail
    }
    d = target_div_neg_zero_by_neg_nonzero();
    if (!is_positive_zero(d)) {
        return 26;  // fail
    }
    d = target_div_pos_non_zero_by_neg_zero();
    if (d >= -1.79e308) {  // should be negative infinity
        return 27;         // fail
    }
    d = target_div_neg_nonzero_by_zero();
    if (d >= -1.79e308) {  // should be negative infinity
        return 28;         // fail
    }
    d = target_div_neg_nonzero_by_neg_zero();
    if (d <= 1.79e308) {  // should be infinity
        return 29;        // fail
    }
    d = target_div_negative_underflow();
    if (!is_negative_zero(d)) {
        return 30;
    }
    d = target_negate_neg_zero();
    if (!is_positive_zero(d)) {
        return 31;  // fail
    }
    d = target_negate_calculated_zero();
    if (!is_negative_zero(d)) {
        return 32;  // fail
    }
    return 0;  // success
}