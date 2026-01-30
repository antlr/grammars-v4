long add_variables(long x, long y, int z);

int main(void) {
    /* Allocate several stack variables of different sizes;
     * in our implementation, this will allocate 20 bytes on the stack */
    long x = 3;
    long y = 4;
    int z = 5;

    /* Test that we can make function calls (i.e. that stack is aligned correctly) */
    return add_variables(x, y, z);
}