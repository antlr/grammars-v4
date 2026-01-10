/* Test subscripting multi-dimensional arrays */

// read an element through a nested subscript
int read_nested(int nested_arr[2][3], int i, int j, int expected) {
    return (nested_arr[i][j] == expected);
}

// write through a nested subscript
int write_nested(int nested_arr[2][3], int i, int j, int new_val) {
    nested_arr[i][j] = new_val;
    return 0;
}

// read through a more complex index
int read_nested_negated(int (*nested_arr)[3], int i, int j, int expected) {
    return (nested_arr[-i][j] == expected);
}


// get address of nested subscript
int get_nested_addr(int nested_arr[2][3], int i, int j, int *expected) {
    return &nested_arr[i][j] == expected;
}

// nested access to a static array
static int nested_arr[4][3][5] = {
    {{1, 2}, {3}},
    {{4}, {5}}
};

int read_static_nested(int i, int j, int k, int expected) {
    return nested_arr[i][j][k] == expected;
}

// write a nested element using more complex expression to get array
int (*get_array(void))[3][5] {
    return nested_arr;
}

int write_nested_complex(int i, int j, int k, int val) {
    get_array()[i][j][k] = val;
    return 0;
}

// only subscript first dimension to return pointer to sub-array
int *get_subarray(int nested[2][3], int i) {
    return nested[i];
}

int main(void) {
    int nested_arr[2][3] = {{1, 2, 3}, {4, 5, 6}};
    if (!read_nested(nested_arr, 1, 2, 6)) {
        return 1;
    }

    write_nested(nested_arr, 1, 2, -1);
    if (nested_arr[1][2] != -1) {
        return 2;
    }

    if (!read_nested_negated(nested_arr + 2, 2, 0, 1)) {
        return 3;
    }

    int *ptr = (nested_arr[0]) + 1;
    if (!get_nested_addr(nested_arr, 0, 1, ptr)) {
        return 4;
    }

    if (!read_static_nested(1, 1, 0, 5)) {
        return 5;
    }

    write_nested_complex(0, 2, 3, 111);
    if (get_array()[0][2][3] != 111) {
        return 6;
    }

    int *row_1 = get_subarray(nested_arr, 1);
    if (row_1 + 1 != &nested_arr[1][1]) {
        return 7;
    }

    return 0;
}