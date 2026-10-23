int main(void) {
    /* This abstract declarator is malformed.
     * Pointer declarators like * cannot appear after
     * parenthesized expressions
     */
    (int (*)*) 10;
    return 0;
}