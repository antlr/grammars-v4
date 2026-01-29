int main(void) {
    /* Define constant doubles in a few different formats,
     * and make sure we can lex all of them.
     * Note that these can all be represented exactly,
     * without rounding.
     */

    /* Several ways to define 1 */
    double a = 1.0;
    double b = 1.;
    double c = 1E0;
    double d = .01e+2;

    /* Make sure they all have the correct value */
    if (! (a == b && a == c && a == d) )
        return 1;
    if (a + b + c + d != 4.0)
        return 2;

    /* Several ways to define .125 */
    double e = .125;
    double f = 12.5e-2;
    double g = 125.E-3;
    double h = 1250000000e-10;

    /* Make sure they all have the correct value */
    if (! (e == f && e == g && e == h) )
        return 3;
    if (e + f + g + h != 0.5)
        return 4;

    return 0;

}
