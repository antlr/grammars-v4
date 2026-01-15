int switch_statement(int i) {
    switch(i) {
        case 0: return 0;
        /* because i is an int, the constant for
         * each case will be converted to an int.
         * 17179869184 (equal to 2^34) will be converted
         * to 0, which conflicts with the previous case.
         */
        case 17179869184: return 0;
        default: return 1;
    }
}

int main(void) {
    return switch_statement(0);
}