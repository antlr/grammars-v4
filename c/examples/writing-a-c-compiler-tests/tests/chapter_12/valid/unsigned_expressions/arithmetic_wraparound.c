/* Test that unsigned arithmetic operations wrap around */

unsigned int ui_a;
unsigned int ui_b;

unsigned long ul_a;
unsigned long ul_b;

int addition(void) {
    // ui_a = UINT_MAX - 2
    // ui_b = 3
    // result wraps around to 0
    return ui_a + ui_b == 0u;
}

int subtraction(void) {
    // ul_a = 10
    // ul_b = 20
    /* ul_a - ul_b wraps around to 2^64 - 10 */
    return (ul_a - ul_b == 18446744073709551606ul);
}

int neg(void) {
    // ul_a = 1ul
    // negating this wraps around to 2^64 - 1, or ULONG_MAX
    return -ul_a == 18446744073709551615UL;
}

int main(void) {
    ui_a = 4294967293u;
    ui_b = 3u;
    if (!addition()) {
        return 1;
    }

    ul_a = 10ul;
    ul_b = 20ul;
    if (!subtraction()) {
        return 2;
    }

    ul_a = 1ul;
    if (!neg()) {
        return 3;
    }

    return 0;

}