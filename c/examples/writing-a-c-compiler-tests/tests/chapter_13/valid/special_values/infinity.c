/* Test our handling of positive and negative infinity */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-range"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

/* This value should be rounded to infinity */
double inf = 2e308;
/* This should round to the largest finite double */
double very_large = 1.79E308;
double zero = 0.0;
int main(void) {

    /* Rounding constants to infinity */

    // 11e330 should be rounded to infinity
    if (inf != 11e330) {
        return 1;
    }

    /* Infinity compares greater than any finite number */
    if (inf <= very_large) {
        return 2;
    }

    /* Calculations that result in infinity */

    /* Multiplication result is too large to represent as finite value */
    if(very_large * 10.0 != inf) {
        return 3;
    }

    /* 1/0 is infinity */
    if (1.0 / zero != inf) {
        return 4;
    }

    /* Negative infinity */
    double negated_inf = -inf;
    double negated_inf2 = -1.0 / zero;

    /* Negative infinity compares less than any finite number */
    if (negated_inf >= -very_large) {
        return 5;
    }

    if (negated_inf != negated_inf2) {
        return 6;
    }

    return 0;
}