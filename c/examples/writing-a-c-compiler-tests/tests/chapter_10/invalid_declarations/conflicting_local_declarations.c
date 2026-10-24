int main(void) {
    /* These two declarations conflict;
     * they declare the same identifier in the same
     * scope, but they don't refer to the same object,
     * since neither one has linkage.
     */
    int x = 1;
    static int x;
    return x;
}