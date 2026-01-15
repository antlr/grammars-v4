int main(void) {
    /* Initialize and then update a mix of variables,
     * to check that we allocate enough stack space for each of them,
     * and writing to one doesn't clobber another.
     * Identical to chapter 11 long_and_int_locals but with some unsigned integers
     */

    unsigned long a = 8589934592ul; // this number is outside the range of int
    int b = -1;
    long c = -8589934592l; // also outside the range of int
    unsigned int d = 10u;

    /* Make sure every variable has the right value */
    if (a != 8589934592ul) {
        return 1;
    }
    if (b != -1){
        return 2;
    }
    if (c != -8589934592l) {
        return 3;
    }
    if (d != 10u) {
        return 4;
    }

    /* update every variable */
    a = -a;
    b = b - 1;
    c = c + 8589934594l;
    d = d * 268435456u; // result is between INT_MAX and UINT_MAX

    /* Make sure the updated values are correct */
    if (a != 18446744065119617024ul) {
        return 5;
    }
    if (b != -2) {
        return 6;
    }
    if (c != 2) {
        return 7;
    }
    if (d != 2684354560u) {
        return 8;
    }

    return 0;
}