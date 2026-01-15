/* Test conversions from unsigned integer types to doubles */
double uint_to_double(unsigned int ui) {
    return (double) ui;
}

double ulong_to_double(unsigned long ul) {
    return (double) ul;
}

int main(void) {

    // uint that's smaller than INT_MAX
    if (uint_to_double(1000u) != 1000.0) {
        return 1;
    }

    // uint that's larger than INT_MAX, so we can't just use cvtsi2sd
    if (uint_to_double(4294967200u) != 4294967200.0) {
        return 2;
    }

    // ulong that's smaller than LONG_MAX
    if (ulong_to_double(138512825844ul) != 138512825844.0) {
        return 3;
    }

    // ulong that's larger than LONG_MAX
    if (ulong_to_double(10223372036854775816ul) != 10223372036854775808.0) {
        return 4;
    }

    /* To test our rounding-to-odd implementation, use the unsigned long from
     * "Converting an Unsigned Integer to a double" in Chapter 13
     * and other values illustrates in Figure 13-7
     */

    /* This value is exactly between 9223372036854775808.0 and 9223372036854777856.0
     * Using ties-to-even rounding, we'll round it down to
     * 9223372036854775808.0, which has an even significand
     */
    if (ulong_to_double(9223372036854776832ul) != 9223372036854775808.0) {
        return 5;
    }

    /* This value is closer to 9223372036854777856.0 than 9223372036854775808.0,
     * so we should round up.
     */
    if (ulong_to_double(9223372036854776833ul) != 9223372036854777856.0) {
        return 6;
    }

    /* This value is closer to 9223372036854775808.0 than 9223372036854777856.0,
     * so round down */
    if (ulong_to_double(9223372036854776831ul) != 9223372036854775808.0) {
        return 7;
    }

    /* This value is closer to 9223372036854775808.0 than 9223372036854777856.0,
     * so round down */
    if (ulong_to_double(9223372036854776830ul) != 9223372036854775808.0) {
        return 8;
    }

    return 0;
}