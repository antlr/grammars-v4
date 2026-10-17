union u {
    int arr[3];
    double d;
};

union u get_union(void) {
    union u result = {{1, 2, 3}, 4.0};
    return result;
}

int main(void) {
    // invalid - can't get address of get_union().arr b/c it's not an lvalue
    // even though it has temporary lifetime
    int *ptr[3] = &get_union().arr;
    return 0;
}