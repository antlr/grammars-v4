/* The result of an assignment statement is not an lvalue,
 * so it's illegal to take its address.
 */
int main(void) {
    int x = 0;
    int y = 0;
    int *ptr = &(x = y);
    return 0;
}