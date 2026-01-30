/* When an int is used the controlling expression of a switch statement,
 * the constant in each case statement should be converted to an int.
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wswitch"
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

int switch_on_int(int i) {
    switch(i) {
        case 5:
            return 0;
        // this is 2^33; it will be truncated to int 0
        case 8589934592l: // case 0:
            return 1;
        // this is 2^35 - 1; it will be truncated to -1
        case 34359738367: // case -1:
            return 2;
        default:
            return 3;
    }
}

int main(void) {
    /* Call switch_on_int once for each case, validate
     * that we get the expected result
     */
    if (switch_on_int(5) != 0)
        return 1;
    if (switch_on_int(0) != 1)
        return 2;
    if (switch_on_int(-1) != 2)
        return 3;
    /* 17179869184 is 2^34; it will be truncated to 0
     * when passed as a parameter to switch_on_int
     */
    if (switch_on_int(17179869184) != 1)
        return 4;

    return 0;
}