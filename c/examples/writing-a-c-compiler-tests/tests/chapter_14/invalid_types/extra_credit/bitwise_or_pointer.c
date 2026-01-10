/* It's illegal to apply bitwise operations to pointers */
int main(void) {
    int *x = 0;
    int *y = 0;
    x | y;
    return 0;
}