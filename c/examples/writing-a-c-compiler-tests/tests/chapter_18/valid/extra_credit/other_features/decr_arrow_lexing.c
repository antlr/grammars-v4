// Make sure we correctly parse postfix -- followed by >
int main(void) {
    int arr[3] = {0, 1, 2};
    int *ptr = arr + 2;
    if(ptr-->arr) {
        return 0; // success
    }
    return 1; // fail
}