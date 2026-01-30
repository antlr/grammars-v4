// The result of a compound assignment expression isn't an lvalue, so you can't
// take its address with &
int main(void) {
    int i = 100;
    int *ptr = &(i += 200);
    return 0;
}