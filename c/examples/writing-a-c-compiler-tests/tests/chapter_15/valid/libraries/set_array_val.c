int set_nth_element(double *arr, int idx) {
    /* Validate current values */
    for (int i = 0; i < 5; i = i + 1) {
        if (arr[i]) {
            return 1;
        }
    }
    arr[idx] = 8;
    return 0;
}

int set_nested_element(int (*arr)[2], int i, int j) {
    for (int x = 0; x < 3; x = x + 1) {
        for (int y = 0; y < 2; y = y + 1) {
            int expected = -10 + 2*x + y;
            if (arr[x][y] != expected) {
                return 4;
            }
        }
    }
    arr[i][j] = 10;
    return 0;
}