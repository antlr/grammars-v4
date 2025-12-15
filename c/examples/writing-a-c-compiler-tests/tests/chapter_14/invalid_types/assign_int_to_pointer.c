/* It's illegal to assign an integer to a pointer,
 * because an integer cannot be implicitly converted
 * to pointer type
 */
int main(void) {
    int *x;
    x = 10;
    return 0;
}