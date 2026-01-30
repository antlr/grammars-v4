int main(void) {
    /* initializing a tests the rewrite rule for
     * movq $large_const, memory_address
     */
    long a = 4294967290l;
    long b = 0l;
    /* Assign the value of one long variable
     * (which is too large for an int to represent)
     * to another long variable
     */
    b = a;
    return (b == 4294967290l);
}