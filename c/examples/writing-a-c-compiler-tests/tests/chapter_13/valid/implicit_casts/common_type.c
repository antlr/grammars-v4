/* Test that we correctly find the common type in expressions involving doubles */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wimplicit-const-int-float-conversion"
#else
#pragma GCC diagnostic ignored "-Wsign-compare"
#endif
#endif

int lt(double d, long l) {
    // l is implicitly converted to a double
    return d < l;
}

double tern_double_flag(double flag) {
    /* Ternary expression where controlling condition is a double
     * You do not have to convert second and third operands to double;
     * instead, we convert them to their common type, which is unsigned long,
     * THEN convert that to a double.
     * Converting -30 to unsigned long gives us 2^64 - 30, or 18446744073709551586.
     * The nearest double to this result is 18446744073709551616.0
     */
    return (double) (flag ? -30 : 10ul);
}

double tern_double_result(int flag) {
    /* The common type of the two operands is double,
     * so if flag is 0, this will return the nearest representable
     * double to 9223372036854777850ul, which is 9223372036854777856.0
     */
    return flag ? 5.0 : 9223372036854777850ul;
}
int ten = 10;
int multiply(void) {
    /* This should promote 10 to a double,
     * calculate 10.75 * 10.0, which is 107.5,
     * and then truncate back to an int, 107.
     * It should not truncate 10.75 to 10 before
     * performing the calculation.
     */
    int i = 10.75 * ten;

    return i == 107;
}

int main(void) {

    /* Comparison:
     * we'll implicitly convert the long argument the nearest double,
     * which is -9007199254751228.0, so these values compare equal
     */
    if (lt(-9007199254751228.0, -9007199254751227l)) {
        return 1;
    }

    /* Ternary expressions */
    if (tern_double_flag(20.0) != 18446744073709551586.0) {
        return 2;
    }

    if (tern_double_flag(0.0) != 10.0) {
        return 3;
    }

    if (tern_double_result(1) != 5.0) {
        return 4;
    }
    if (tern_double_result(0) != 9223372036854777856.0) {
        return 5;
    }

    /* Multiplication */
    if (!multiply()) {
        return 6;
    }
    return 0;
}