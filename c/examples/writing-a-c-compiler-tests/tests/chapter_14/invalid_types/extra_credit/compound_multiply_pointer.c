/* A pointer can't appear as the left or right operand
 * of the *=, /=, or %= operator
 */
int main(void) {
    int *x = 0;
    x *= 2;
    return 0;
}