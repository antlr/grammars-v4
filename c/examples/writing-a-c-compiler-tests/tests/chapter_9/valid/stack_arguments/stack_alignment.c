/* Call functions with both even and odd numbers of stack arguments,
 * to make sure the stack is correctly aligned in both cases.
 */

// these are defined in stack_alignment_check_<platform>.s
// and exit with value -1 if RSP is misaligned or arguments
// don't have the expected values
int even_arguments(int a, int b, int c, int d, int e, int f, int g, int h);

int odd_arguments(int a, int b, int c, int d, int e, int f, int g, int h, int i);

int main(void) {
    /* Allocate an argument on the stack, to check that
     * we properly account for already-allocated stack space
     * when deciding how much padding to add
     */
    int x = 3;
    // call some functions, check stack alignment
    even_arguments(1, 2, 3, 4, 5, 6, 7, 8);
    odd_arguments(1, 2, 3, 4, 5, 6, 7, 8, 9);
    // return x to make sure it hasn't been clobbered
    return x;
}