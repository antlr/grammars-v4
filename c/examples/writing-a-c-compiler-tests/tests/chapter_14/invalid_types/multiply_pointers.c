/* It's illegal to multiply, divide, or take the modulo of pointers */
int main(void) {
    int *x = 0;
    int *y = x;
    (x * y);
    return 0;
}