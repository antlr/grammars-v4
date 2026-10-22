int main(void) {
    /* only one "L" suffix is permitted on a long
     * Note: an "LL" suffix is standard-compliant and indicates
     * a long long constant, but isn't supported by our implementation.
     * Instead, we use an LLL suffix, which is always invalid.
     */
    return 0LLL;
}