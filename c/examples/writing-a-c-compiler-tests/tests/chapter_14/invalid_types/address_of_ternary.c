/* The result of a ternary expression is not
 * an lvalue, even if the second and third operands
 * are both variables, so it's illegal to take its address.
 */
int main(void) {
    int x = 1;
    int y = 2;
    int z = 3;
    int *ptr = &(x ? y : z);
    return 0;
}