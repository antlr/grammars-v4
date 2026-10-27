// Increment and decrement lvalues of character type
int main(void) {
    static char chars[5] = {123, 124, 125, 126, 127};
    if (chars[0]++ != 123) {
        return 1;  // fail
    }

    if (chars[1]-- != 124) {
        return 2;  // fail
    }

    if (++chars[2] != 126) {
        return 3;  // fail
    }

    if (--chars[3] != 125) {
        return 4;  // fail
    }

    // NOTE: this is not undefined because we perform the usual arithmetic
    // conversions before incrementing, then perform implementation-defined
    // conversion back to character type, so there's no integer overflow
    if (++chars[4] != -128) {
        return 5;  // fail
    }

    // validate all 5 elements
    if (chars[0] != 124) {
        return 6;  // fail
    }

    if (chars[1] != 123) {
        return 7;  // fail
    }
    if (chars[2] != 126) {
        return 8;  // fail
    }
    if (chars[3] != 125) {
        return 9;  // fail
    }
    if (chars[4] != -128) {
        return 10;  // fail
    }

    // make sure decrementing CHAR_MIN also wraps around as expected
    signed char c = -128;
    c--;
    if (c != 127) {
        return 11;  // fail
    }

    return 0;  // success
}