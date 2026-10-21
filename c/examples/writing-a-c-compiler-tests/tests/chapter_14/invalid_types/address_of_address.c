/* The result of the & operator is not an lvalue,
 * so it's illegal to take its address
 */
int main(void) {
    int x = 0;
    int *y = &x;
    int **z = &(&x);
    return 0;
}