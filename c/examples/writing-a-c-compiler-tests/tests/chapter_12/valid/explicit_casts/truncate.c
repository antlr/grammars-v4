/* Test truncating wider to narrow types */
int ulong_to_int(unsigned long ul, int expected) {
    int result = (int) ul;
    return (result == expected);
}

int ulong_to_uint(unsigned long ul, unsigned expected) {
    return ((unsigned int) ul == expected);
}

int long_to_uint(long l, unsigned int expected) {
    return (unsigned int) l == expected;
}

int main(void) {
    /* truncate long */

    /* 100 is in the range of unsigned int,
     * so truncating it to an unsigned int
     * will preserve its value
     */
    if (!long_to_uint(100l, 100u)) {
        return 1;
    }

    /* -9223372036854774574 (i.e. -2^63 + 1234) is outside the range of unsigned int,
     * so add 2^32 to bring it within range */
    if (!long_to_uint(-9223372036854774574l, 1234u)) {
        return 2;
    }

    /* truncate unsigned long */

    /* 100 can be cast to an int or unsigned int without changing its value */
    if (!ulong_to_int(100ul, 100)) {
        return 3;
    }

    if (!ulong_to_uint(100ul, 100u)) {
        return 4;
    }

    /* 4294967200 (i.e. 2^32 - 96) can be cast to an unsigned int without changing its value,
     * but must be reduced modulo 2^32 to cast to a signed int
     */
    if (!ulong_to_uint(4294967200ul, 4294967200u)) {
        return 5;
    }

    if (!ulong_to_int(4294967200ul, -96)) {
        return 6;
    }

    /* 1152921506754330624 (i.e. 2^60 + 2^31) must be reduced modulo 2^32
     * to represent as a signed or unsigned int
     */

    if (!ulong_to_uint(1152921506754330624ul, 2147483648u)) { // reduce to 2^31
        return 7;
    }

    if (!ulong_to_int(1152921506754330624ul, -2147483648)){ // reduce to -2^31
        return 8;
    }

    /* truncate unsigned long constant that can't
     * be expressed in 32 bits, to test rewrite rule
     */
    unsigned int ui = (unsigned int)17179869189ul; // 2^34 + 5
    if (ui != 5)
        return 9;

    return 0;
}