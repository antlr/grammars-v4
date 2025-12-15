int f(int i) {
    /* Can't declare the same identifier in the same scope both with and
     * without linkage. (The parameter and variable i in this program
     * are in the same scope.) */
    extern int i;
    return i;
}

int main(void) {
    return 0;
}