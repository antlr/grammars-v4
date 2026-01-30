// If LHS of += or -= is a pointer, RHS must be an integer (not a double or pointer)
int main(void) {
    int arr[3] = {1, 2, 3};
    int *elem = arr;
    elem += 1.0;
    return 0;
}