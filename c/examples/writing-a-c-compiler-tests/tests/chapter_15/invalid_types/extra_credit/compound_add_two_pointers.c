// += and -= operators: RHS must not be a pointer regardless of LHS type
int main(void) {
    int arr[3] = {1, 2, 3};
    int *elem0 = arr;
    int *elem1 = arr + 1;
    elem0 += elem1;
    return 0;
}