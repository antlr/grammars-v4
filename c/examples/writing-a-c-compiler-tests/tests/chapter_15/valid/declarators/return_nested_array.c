/* Declare a function that returns a pointer to an array */

int arr[3] = {1, 1, 1};

int (*foo(int x, int y))[3] {
    arr[1] = x;
    arr[2] = y;
    return &arr;
}

int main(void) {
    int (*arr)[3] = foo(2, 3);
    if (arr[0][0] != 1) {
        return 1;
    }
    if (arr[0][1] != 2) {
        return 2;
    }
    if (arr[0][2] != 3) {
        return 3;
    }
    return 0;
}