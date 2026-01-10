// An array (including a nested array) that decays to a pointer isn't an lvalue,
// so we can't assign to it with ++/--
int main(void) {
    int arr[2][3] = {{1, 2, 3}, {4, 5, 6}};
    arr[2]++;
    return 0;
}