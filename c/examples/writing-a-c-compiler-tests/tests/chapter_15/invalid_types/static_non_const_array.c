int foo(int p) {
    // Because arr has static storage duration,
    // the initializer for each element must be a constant
    static int arr[3] = { p, p + 1, 0};
    return arr[2];
}

int main(void) {
    return foo(5);
}