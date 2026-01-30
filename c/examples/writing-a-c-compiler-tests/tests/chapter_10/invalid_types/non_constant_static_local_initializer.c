int main(void) {
    int a = 1;
    /* b has static storage duration,
     * so its initializer must be constant.
     */
    static int b = a * 2;
    return b;
}