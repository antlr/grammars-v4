// No bitwise operations on pointers, including bitwise compound assignment
// with pointer as right-hand side
int main(void) {
    int *null = 0;
    int x = 100;
    x |= null;
    return 1;
}