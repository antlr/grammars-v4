int main(void) {
    int i = -20;
    int b = 2147483647;
    int c = -5000000;

    /* This statement is evaluated as follows:
     * 1. sign-extend i to a long with value -20
     * 2. add this long to 2147483648, resulting in the long 2147483628,
     * 3. convert this to an int with value 2147483628 (this value
     * can be represented as an int)
     */
    i += 2147483648l;

    // make sure we got the right answer and didn't clobber b
    if (i != 2147483628) {
        return 1;
    }
    if (b != 2147483647) {
        return 2;
    }

    // b /= -2^35 + 1
    // if we try to perform int (rather than long)
    // division, we'll interpret this value as 1 and
    // b's value won't change.
    b /= -34359738367l;
    if (b) { // b's value should be 0
        return 3;
    }

    // make sure we didn't clobber i or c
    if (i != 2147483628) {
        return 4;
    }
    if (c != -5000000) {
        return 5;
    }

    // this result will be outside the range of int; we'll
    // convert it to int in the usual implementation-defined way
    c *= 10000l;
    if (c != 1539607552) {
        return 6;
    }

    return 0;
}