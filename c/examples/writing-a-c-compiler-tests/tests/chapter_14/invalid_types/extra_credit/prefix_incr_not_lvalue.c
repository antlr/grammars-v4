// The result of a prefix ++ or -- operation is not an lvalue,
// so you can't take its address
int main(void) {
    int i = 10;
    int *ptr = &++i;
    return 0;
}