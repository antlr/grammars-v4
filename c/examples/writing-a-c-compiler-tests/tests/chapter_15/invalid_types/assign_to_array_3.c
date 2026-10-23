int main(void) {
    int arr[3] = { 1, 2, 3};
    int (*ptr_to_array)[3];
    // *ptr_to_array has array type, so we can't assign to it
    *ptr_to_array = arr;
}