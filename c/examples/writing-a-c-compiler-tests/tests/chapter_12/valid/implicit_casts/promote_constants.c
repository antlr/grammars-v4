#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wsign-compare"
#endif
/* Make sure we implicitly promote constants from unsigned int to unsigned long
 * as needed, but don't promote int to unsigned int */

/* make this a global variable so we don't
 * optimize away these comparisons in Part III
 */
long negative_one = 1l; // can't use negative static initializers; negate this in main
long zero = 0l;

int main(void) {

    negative_one = -negative_one;
    /* 2^36 can't be represented as an unsigned int,
     * so it will be promoted to an unsigned long;
     * when we compare this to -1l, we'll convert -1l to
     * an unsigned long with value ULONG_MAX
     */
    if (68719476736u >= negative_one) {
        return 1;
    }

    /* The integer constant with value 2^31 + 10
     * is promoted to signed long, not an unsigned int,
     * so negating it gives us a negative signed value.
     * If it were promoted to an unsigned int, comparing it to 0l
     * would require us to zero-extend it and we'd get a positive value.
     */
    if (-2147483658 >= zero) {
        return 2;
    }

    /* constants with ul suffix are always treated as unsigned long, not unsigned int
     * If these constants were interpreted as unsigned ints, addition would wrap around to 0
     */
    if (!(3ul + 4294967293ul)) {
        return 3;
    }

    return 0;
}