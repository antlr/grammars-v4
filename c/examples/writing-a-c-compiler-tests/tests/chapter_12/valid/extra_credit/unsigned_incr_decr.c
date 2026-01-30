// test ++/-- with unsigned values (including wraparound)
int main(void) {
    unsigned int i = 0;

    // Postfix --, including wraparound
    if (i-- != 0) {
        return 1;
    }
    if (i != 4294967295U) { // wraparound from 0 to UINT_MAX
        return 2;
    }

    // Prefix --
    if (--i != 4294967294U) {
        return 3;
    }
    if (i != 4294967294U) {
        return 4;
    }

    unsigned long l = 18446744073709551614UL;
    // Postfix ++
    if (l++ != 18446744073709551614UL) {
        return 5;
    }
    if (l != 18446744073709551615UL) {
        return 6;
    }
    if (++l != 0) { // wraparound from ULONG_MAX to 0
        return 7;
    }
    if (l != 0) {
        return 8;
    }
    return 0; // success
}