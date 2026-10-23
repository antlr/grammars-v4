int main(void) {
    /* This declarator is malformed.
     * Pointer declarators  like * cannot appear after
     * parenthesized expressions
     */
    int (*)* y;
    return 0;
}