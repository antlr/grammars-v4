int main(void) {
    /* only one "L" suffix is permitted on a long
     * Note: an "LL" suffix is standard-compliant and indicates
     * a long long constant, which our implementation doesn't support,
     * but an "lL" suffix is invalid
     */
    return 0lL;
}