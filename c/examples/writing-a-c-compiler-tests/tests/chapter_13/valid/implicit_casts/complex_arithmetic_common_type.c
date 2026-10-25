/* Test that when we evaluate a complex expression,
 * we perform the usual arithmetic conversions correctly
 * for each sub-expression
 */

unsigned long ul = 10000ul;
int main(void) {

    int i = -50;
    /* When we calculate ul + i, we first promote
     * i to an unsigned long with value 2^64 - 50,
     * so ul + i will wrap around to 9950.
     * We'll promote this value to a double
     * and then multiply it by 3.125, resulting in
     * 31093.75
     */
    double d = (ul + i) * 3.125;
    return d == 31093.75;
}