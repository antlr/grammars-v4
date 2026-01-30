int main(void) {

    /* A constant with an l suffix always has long type */

    // if we parse these as ints, this addition will probably overflow and be negative
    if (2147483647l + 2147483647l < 0l) {
        return 1;
    }
    /* if a constant is too large to store as an int,
     * it's automatically converted to a long, even if it
     * doesn't have an 'l' suffix
     * if we parsed 19327352832 as an int, it would be negative
     */
    if (19327352832 < 100l) { // 19327352832 == 2^34 + 2^31
        return 2;
    }
    return 0;
}