/* Test conversions from narrower to wider types */

int int_to_ulong(int i, unsigned long expected) {
    unsigned long result = (unsigned long) i;
    return result == expected;
}

int uint_to_long(unsigned int ui, long expected) {
    long result = (long) ui;
    return result == expected;
}

int uint_to_ulong(unsigned ui, unsigned long expected){
    return (unsigned long) ui == expected;
}

int main(void) {
    /* Converting a positive int to an unsigned long preserves its value */
    if (!int_to_ulong(10, 10ul)) {
        return 1;
    }

    /* When you convert a negative int to an unsigned long,
     * add 2^64 until it's positive
     */
    if (!int_to_ulong(-10, 18446744073709551606ul)) {
        return 2;
    }

    /* Extending an unsigned int to a signed long preserves its value */
    if (!uint_to_long(4294967200u, 4294967200l)) {
        return 3;
    }

    /* Extending an unsigned int to an unsigned long preserves its value */
    if (!uint_to_ulong(4294967200u, 4294967200ul)) {
        return 4;
    }
    /* Zero-extend constant 4294967200
     * from an unsigned int to an unsigned long
     * to test the assembly rewrite rule for MovZeroExtend
     */
    if ((unsigned long) 4294967200u != 4294967200ul) {
        return 5;
    }
    return 0;
}
