int main(void) {
    double d = 1000.5;
    /* When we perform compound assignment, we convert both operands
     * to their common type, operate on them, and convert the result to the
     * type of the left operand */
    d += 1000;
    if (d != 2000.5) {
        return 1;
    }

    unsigned long ul = 18446744073709551586ul;
    /* We'll promote ul to the nearest double,
     * which is 18446744073709551616,
     * then subtract 1.5 * 10^19, which
     * results in 3446744073709551616.0,
     * then convert it back to an unsigned long
     */
    ul -= 1.5E19;
    if (ul != 3446744073709551616ul) {
        return 2;
    }
    /* We'll promote i to a double, add .99999,
     * then truncate it back to an int
     */
    int i = 10;
    i += 0.99999;
    if (i != 10) {
        return 3;
    }

    return 0;
}