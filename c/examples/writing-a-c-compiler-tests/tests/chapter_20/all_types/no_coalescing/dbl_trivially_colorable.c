/* A simple test that we can allocate all the floating-point
 * pseudos in a program without register pressure.
 */
int target(double x, double y) {
    return 10 - (3.0 * y + x);
}