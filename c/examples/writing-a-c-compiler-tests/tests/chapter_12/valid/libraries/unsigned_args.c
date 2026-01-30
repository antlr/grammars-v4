int accept_unsigned(unsigned int a, unsigned int b, unsigned long c, unsigned long d,
                 unsigned int e, unsigned int f, unsigned long g, unsigned int h,
                 unsigned long i) {
    /* Make sure unsigned arguments are passed correctly */
    if (a != 1u) {
        return 1;
    }
    if (b != 4294967295U) {
        return 2;
    }
    if (c != 18446744073709551615UL) {
        return 3;
    }
    if (d != 9223372036854775808ul) {
        return 4;
    }
    if (e != 2147483648u) {
        return 5;
    }
    if (f != 0u) {
        return 8;
    }
    if (g != 123456u) {
        return 9;
    }
    if (h != 2147487744u) {
        return 10;
    }
    if (i != 9223372041149743104ul) {
        return 11;
    }
    return 0;
}