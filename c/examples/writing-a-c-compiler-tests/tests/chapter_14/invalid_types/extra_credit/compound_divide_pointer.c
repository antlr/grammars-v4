/* A pointer can't appear as the left or right operand
 * of the *=, /=, or %= operator
 */
int main(void) {
    int *x = 0;
    int *y = 0;
    x /= y;
    return 0;
}