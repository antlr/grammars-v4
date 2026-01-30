// Can't switch on expression of array type
int main(void) {
    int arr[3] = {1, 2, 3};
    switch (arr) {
        default:
            return 0;
    }
    return 1;
}