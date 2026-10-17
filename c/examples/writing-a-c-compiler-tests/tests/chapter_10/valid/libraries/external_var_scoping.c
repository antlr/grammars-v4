int read_x(void) {
    //  define x without linkage
    int x = 4;
    if (x == 4) {
        /* declare x with linkage, shadowing previous definition;
         * this refers to the variable defined in the other file.
         */
        extern int x;
        return x;
    } else {
        return -1;
    }
}