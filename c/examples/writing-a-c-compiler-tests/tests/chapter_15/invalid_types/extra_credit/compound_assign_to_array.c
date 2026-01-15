// An array that decays to a pointer is not an lvalue, so you can't
// assign to it with += or -=
int main(void) {
    int arr[3] = {1, 2, 3};
    arr -= 1;
    0;
}