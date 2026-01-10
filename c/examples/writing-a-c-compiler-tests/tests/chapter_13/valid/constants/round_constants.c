/* Make sure we round constant tokens to the nearest representable double */

int main(void) {
    /* Both these values should round to
     * 1.000000000000000444089209850062616169452667236328125.
     * (I got these numbers from from example 2 in
     * https://www.exploringbinary.com/17-digits-gets-you-there-once-youve-found-your-way/)
     */
    if (1.00000000000000033306690738754696212708950042724609375 != 1.0000000000000004) {
        return 1;
    }

    /* Test for double-rounding errors that could result
     * if your compiler uses an 80-bit (aka extended-precisions)
     * floating-point representation internally
     */

    /* If 9223372036854776832.5 is converted to double precision (64 bits),
     * it will be rounded to 9223372036854777856.0,
     * so this function will return 1
     *
     * if your compiler uses 80-bit precision internally,
     * it will be rounded to 9223372036854776832,
     * which would later be rounded down to the 64-bit value
     * 9223372036854775808.0, so this function will return 0.
     *
     * This incorrect double rounding would be equivalent to:
     *     return ( (double) 9223372036854776832.5l == 9223372036854777856.0);
     * You can try this out and see that it returns 0.
     * (note that the "l" suffix above indicates the "long double" type,
     * which has 80-bit precision)
     */
    if (9223372036854776832.5 != 9223372036854777856.0) {
        return 2;
    }

    return 0;
}