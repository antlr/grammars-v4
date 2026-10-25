/* Test basic arithmetic operations on unsigned integers
 * None of these operations wrap around; that's tested separately in arithmetic_wraparound
 */

unsigned int ui_a;
unsigned int ui_b;

unsigned long ul_a;
unsigned long ul_b;

int addition(void) {
    // ui_a = 10u;

    /* Test out rewrite rule for addition;
     * even when adding numbers we interpret as unsigned,
     * the 'add' instruction can only handle immediate values
     * that fit into a signed int
     * second operand here is 2147483653, larger than INT_MAX
     */
    return (ui_a + 2147483653u == 2147483663u);
}

int subtraction(void) {
    // ul_a = 2^64 - 2^30
    // ul_b = 1000
    return (ul_a - ul_b == 18446744072635808792ul);
}

int multiplication(void) {
    // ui_a = 2^30
    // ui_b = 3
    // product fits in unsigned int but not int
    return (ui_a * ui_b == 3221225472u);
}

int division(void) {
    // ui_a = 100
    // ui_b = 4294967294

    /* ui_a/ui_b is 0.
     * If you interpreted these as signed values, ui_b would be -2
     * and ui_a / ui_b would be -50
     */
    return (ui_a / ui_b == 0);
}

int division_large_dividend(void) {
    // ui_a = 4294967294
    // ui_b = 2147483647

    /* upper bit of ui_a is set, so this tests
     * that we zero-extended dividend into EDX
     * instead of sign extending it
     */

    return (ui_a / ui_b == 2);
}

int division_by_literal(void) {
    // ul_a = 1099511627775, i.e. 2^40 - 1
    // exercise assembly rewrite rule for div $const
    return (ul_a / 5ul == 219902325555ul);
}

int remaind(void) {
    // ul_a = 100
    // ul_b = 18446744073709551605

    /* ul_b % ul_a is 5.
     * If you interpreted these as signed values, ul_b would be -11
     * and ul_b % ul_a would also be -11.
     */

    return (ul_b % ul_a == 5ul);
}
int complement(void) {
    // ui_a = UINT_MAX
    return (~ui_a == 0);
}

int main(void) {

    ui_a = 10u;
    if (!addition()) {
        return 1;
    }

    ul_a = 18446744072635809792ul;
    ul_b = 1000ul;
    if (!subtraction()) {
        return 2;
    }

    ui_a = 1073741824u;
    ui_b = 3u;
    if (!multiplication()) {
        return 3;
    }

    ui_a = 100u;
    ui_b = 4294967294u;

    if (!division()) {
        return 4;
    }

    ui_a = 4294967294u;
    ui_b = 2147483647u;
    if (!division_large_dividend()) {
        return 5;
    }

    ul_a = 1099511627775ul;
    if (!division_by_literal()) {
        return 6;
    }

    ul_a = 100ul;
    ul_b = 18446744073709551605ul;
    if (!remaind()) {
        return 7;
    }

    ui_a = 4294967295U;
    if (!complement()) {
        return 8;
    }

    return 0;
}