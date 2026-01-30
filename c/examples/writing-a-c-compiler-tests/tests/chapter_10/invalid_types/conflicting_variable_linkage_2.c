int main(void) {
    /* A local variable with no linkage */
    int x = 3;
    {
        /* Because no other x identifier
         * with any linkage has been declared,
         * the 'extern' keyword gives this external
         * linkage.
         */
        extern int x;
    }
    return x;
}

/* This has internal linkage, so it conflicts
 * with the previous declaration of x.
 */
static int x = 10;