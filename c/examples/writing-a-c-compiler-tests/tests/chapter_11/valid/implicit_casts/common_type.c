/* Test that we correctly find the common type in binary expressions */

long l;
int i;

int addition(void) {
    // l = 2147483653
    // i = 10

    /* The common type of i and l is long, so we should
     * promote i to a long, then perform addition.
     * If we instead converted l to an int, its value would be
     * -2147483643, and the result of i + l would be -2147483633
     */
    long result = i + l;
    return (result == 2147483663l);
}

int division(void) {
    // l = 2147483649l
    // i = 10l

    /* The common type of i and l is long.
     * Therefore, we should promote i to a long,
     * then divide (resulting in 214748364),
     * then convert back to an int (which can be done without
     * changing the result's value, since 214748364 is within
     * the range of int.)

     * If instead we truncated l to an int before performing division,
     * the result would be -2147483647 / 10, or -214748364.
     */
    int int_result = l / i;
    return (int_result == 214748364);
}

int comparison(void) {
    // i = -100
    // l = 2147483648, i.e. 2^31

    /* Make sure we convert i to a long instead of converting l to an int.
     * If we convert l to an int its value will be -2147483648,
     * which is smaller than -100.
     */
    return (i <= l);
}

int conditional(void) {
    // l = 8589934592l, i.e. 2^33
    // i = 10;

    /* When a conditional expression includes both int and long branches,
     * make sure the int type is promoted to a long, rather than the long being
     * converted to an int
     */
    long result = 1 ? l : i;
    return (result == 8589934592l);
}

int main(void) {
    // Addition
    l = 2147483653;
    i = 10;
    if (!addition()) {
        return 1;
    }

    // Division
    l = 2147483649l;
    if (!division()) {
        return 2;
    }

    // Comparison
    i = -100;
    l = 2147483648; // 2^31
    if (!comparison()) {
        return 3;
    }

    // Conditional
    l = 8589934592l; // 2^33
    i = 10;
    if (!conditional()) {
        return 4;
    }

    return 0;
}