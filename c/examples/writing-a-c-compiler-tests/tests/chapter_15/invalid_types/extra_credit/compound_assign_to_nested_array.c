// An array (including a nested array) that decays to a pointer isn't an lvalue,
// so we can't assign to it with +=
int main(void) {
    long arr[2][2] = {{1, 2}, {3, 4}};
    arr[1] += 1;
    return 0;
}