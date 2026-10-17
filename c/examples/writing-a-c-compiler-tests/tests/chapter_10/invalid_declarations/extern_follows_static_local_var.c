int main(void) {
    /* This declaration has no linkage. */
    static int x  = 0;
    /* This declaration has external linkage,
     * so it conflict with the previous declaration
     * that has no linkage. */
    extern int x;
    return x;
}