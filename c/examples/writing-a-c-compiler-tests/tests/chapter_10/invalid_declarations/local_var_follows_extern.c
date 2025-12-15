int i = 10;

int main(void) {
    /* This declaration has external linkage,
     * so it refers to the static global variable
     * defined above. */
    extern int i;
    /* This declaraiton has no linkage,
     * so it conflicts with the previous one.
     */
    int i;
    return i;
}