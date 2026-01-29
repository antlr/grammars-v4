int main(void) {
    unsigned int x = -1u; // 2^32 - 1
    /* 1. convert x to a signed long, which preserves its value
     * 2. divide by -10, resulting in -429496729
     * 3. convert -429496729 to an unsigned int by adding 2^32
     */
    x /= -10l;

    return (x == 3865470567u);
}