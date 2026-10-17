/* A pointer can't appear as the left or right operand
 * of the *=, /=, or %= operator
 */
int main(void) {
    int i = 10;
    int *ptr = &i;
    i %= ptr;
    return 0;
}