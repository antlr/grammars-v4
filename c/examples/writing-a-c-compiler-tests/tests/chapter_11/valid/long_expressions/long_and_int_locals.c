int main(void) {
    /* Initialize and then update a mix of long and int variables,
     * to check that we allocate enough stack space for each of them,
     * and writing to one doesn't clobber another */

    long a = 8589934592l; // this number is outside the range of int
    int b = -1;
    long c = -8589934592l; // also outside the range of int
    int d = 10;

    /* Make sure every variable has the right value */
    if (a != 8589934592l) {
        return 1;
    }
    if (b != -1){
        return 2;
    }
    if (c != -8589934592l) {
        return 3;
    }
    if (d != 10) {
        return 4;
    }

    /* update every variable */
    a = -a;
    b = b - 1;
    c = c + 8589934594l;
    d = d + 10;

    /* Make sure the updated values are correct */
    if (a != -8589934592l) {
        return 5;
    }
    if (b != -2) {
        return 6;
    }
    if (c != 2) {
        return 7;
    }
    if (d != 20) {
        return 8;
    }

    return 0;
}