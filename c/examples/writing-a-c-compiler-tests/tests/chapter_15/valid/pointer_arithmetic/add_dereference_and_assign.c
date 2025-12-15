/* Test that you can assign to any dereferenced pointer,
 * including pointers resulting from pointer arithmetic */
int main(void) {
    int arr[2] = {1, 2};
    // dereferenced expressions, including dereferenced results of
    // pointer arithmetic, are valid lvalues
    *arr = 3;
    *(arr + 1) = 4;
    if (arr[0] != 3) {
        return 1;
    }

    if (arr[1] != 4) {
        return 2;
    }
    return 0;
}