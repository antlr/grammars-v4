/* Test that we promote character types to integers when we're required to */

int add_chars(char c1, char c2, char c3) {
    // these are all promoted, so this won't overflow
    // even if the result is greater than CHAR_MAX
    return c1 + c2 + c3;
}

int negate(unsigned char uc) {
    // this is promoted to an int before being negated
    // so its value will never wrap around
    return -uc;
}

int complement(unsigned char uc) {
    // this is promoted to an int before being negated
    // so its value will never wrap around
    return ~uc;
}

int add_then_div(signed char a, signed char b, signed char c) {
    // all operands are promoted to int, so intermediate result can't overflow
    return (a + b) / c;
}

int mixed_multiply(signed char s, unsigned char u) {
    // both chars are converted to int instead of converting signed to unsigned or vice versa
    return s * u;
}

signed char decrement(signed char s) {
    s = s - 1;
    return s;
}

int main(void) {
    char a = 100;
    char b = 109;
    // make sure this doesn't overflow
    if (add_chars(a, a, b) != 309) {
        return 1;
    }

    unsigned char one = 1;
    // because of integer promotion, this won't wrap around to 255
    if (negate(one) != -1) {
        return 2;
    }

    // because of integer promotion, this won't wrap around to 254
    if (complement(one) != -2) {
        return 3;
    }

    signed char w = 127;
    signed char x = 3;
    signed char y = 2;
    // we should promote all types to int so that intermediate result (127 + 3)
    // doesn't overflow; final result (127 + 3) / 2 will fit in signed char
    if (add_then_div(w, x, y) != 65)
        return 4;

    // operating on signed/unsigned chars, both are converted to int
    signed char sc = -3;
    unsigned char uc = 250;
    if (mixed_multiply(sc, uc) != -750)
        return 5;

    sc = -128; // INT_MIN
    if (sc != -128) {
        return 6;
    }

    // subtracting one from this char is well-defined:
    // we'll first promote to int, then decrement, then truncate back to char
    if (decrement(sc) != 127) {
        return 7;
    }

    return 0;
}
