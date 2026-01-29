int main(void) {
    /* It's illegal to apply bitwise & to doubles */
    double d = 10.0 & -1;
    return 0;
}