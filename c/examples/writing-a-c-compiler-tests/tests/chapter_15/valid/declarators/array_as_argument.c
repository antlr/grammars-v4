/* Test that array types in parameters are converted to pointer types */

#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Warray-parameter"
#endif

/* The type of 'a' will be adjusted to (int *) */
int array_param(int a[5]) {
    a[4] = 0;
    return 0;
}

/* Now try a multi-dimensional array; the type of 'a' will be adjusted to int (*)[3] */
int nested_array_param(int a[2][3]) {
    a[1][1] = 1;
    return 0;
}

/* It's okay to redeclare a function with a different outermost array dimension,
 * because that dimension is ignored
 */
int array_param(int a[2]);

int nested_array_param(int (*a)[3]);

int main(void) {

    // Make sure we adjust parameters in local function declarations too
    int array_param(int a[6]);
    int nested_array_param(int a[5][3]);


    // call array_param and make sure it works as expected
    int arr[8] = {8, 7, 6, 5, 4, 3, 2, 1};
    array_param(arr);
    if (arr[4]) {
        return 1;
    }

    // check the other elements too
    for (int i = 0; i < 8; i = i + 1) {
        if (i != 4 && arr[i] != 8 - i)
            return 2;
    }

    // call nested_array_param and make sure it works as expected
    int nested_arr[4][3] = { {-1, -1, -1}, {-2, -2, -2}, {-3, -3, -3}, {-4, -4, -4}};

    nested_array_param(nested_arr);
    if (nested_arr[1][1] != 1) {
        return 3;
    }

    // check other elements
    for (int i = 0; i < 4; i = i + 1) {
        int expected = -1 - i;
        for (int j = 0; j < 3; j = j + 1) {
            if ((i != 1 || j != 1) &&
                (nested_arr[i][j] != expected)) {
                    return 4;
            }
        }
    }

    return 0;
}

int array_param(int *a);