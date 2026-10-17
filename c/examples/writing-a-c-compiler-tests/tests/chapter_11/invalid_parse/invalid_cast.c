int main(void) {
    /* A cast expression can only contain type specifiers,
     * not storage class specifiers
     */
    return (static int) 10;
}