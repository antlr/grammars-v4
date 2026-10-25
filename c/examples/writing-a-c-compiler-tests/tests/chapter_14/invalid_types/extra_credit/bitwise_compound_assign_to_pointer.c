// No bitwise operations on pointers, including bitwise compound assignment
int main(void) {
    int x = 0;
    int *ptr = &x;
    ptr &= 0;
    return 0;
}