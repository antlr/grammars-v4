/* Test initializing nested arrays with automatic storage duration */

/* A fully initialized array of constants */
int test_simple(void) {
    int arr[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};

    // check the value of each element
    for (int i = 0; i < 3; i = i + 1) {
        for (int j = 0; j < 3; j = j + 1) {
            if (arr[i][j] != i * 3 + j + 1) {
                return 0;
            }
        }
    }

    return 1;  // success
}

/* A partially initialized array of constants.
 * Elements that aren't explicitly initialized
 * (including nested arrays) should be zeroed out.
 * */
int test_partial(void) {
    // explicitly initialize only the first half of each array,
    // at each dimension
    int first_half_only[4][2][6] = {
        {{1, 2, 3}},  // first_half_only[0][0][0-2]
        {{4, 5, 6}}   // first_half_only[1][0][0-2]
    };

    int expected = 1;
    for (int i = 0; i < 4; i = i + 1) {
        for (int j = 0; j < 2; j = j + 1) {
            for (int k = 0; k < 6; k = k + 1) {
                int val = first_half_only[i][j][k];
                if (i > 1 || j > 0 || k > 2) {
                    // this wasn't explicitly initialized, should be zero
                    if (val) {
                        return 0;
                    }
                } else {
                    if (val != expected) {
                        return 0;
                    }
                    expected = expected + 1;
                }
            }
        }
    }

    return 1;  // success
}

/* elements in a compound initializer may include non-constant expressions
 * and expressions of other types, which are converted to the right type
 * as if by assignment */
int test_non_constant_and_type_conversion(void) {
    // first let's define some value (that can't be copy propagated
    // or constant-folded away in Part III)
    extern unsigned int three(void);
    static int x = 2000;
    int negative_four = -4;
    int *ptr = &negative_four;

    double arr[3][2] = {
        {x, x / *ptr},
        {three()},
    };

    if (arr[0][0] != 2000.0 || arr[0][1] != -500.0 || arr[1][0] != 3.0) {
        return 0;
    }

    if (arr[1][1] || arr[2][0] || arr[2][1]) {
        return 0;
    }

    return 1;  // success
}

// helper function for previous test
unsigned int three(void) {
    return 3u;
}

/* Initializing an array must not corrupt other objects on the stack. */
long one = 1l;
int test_preserve_stack(void) {
    int i = -1;

    /* Initialize with expressions of long type - make sure they're truncated
     * before being copied into the array.
     * Also use an array of < 16 bytes so it's not 16-byte aligned, so there are
     * quadwords that include both array elements and other values.
     * Also leave last element uninitialized; in assembly, we should set it to
     * zero without overwriting what follows
     */
    int arr[3][1] = {{one * 2l}, {one + three()}};
    unsigned int u = 2684366905;

    if (i != -1) {
        return 0;
    }

    if (u != 2684366905) {
        return 0;
    }

    if (arr[0][0] != 2 || arr[1][0] != 4 || arr[2][0] != 0) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    if (!test_simple()) {
        return 1;
    }

    if (!test_partial()) {
        return 2;
    }

    if (!test_non_constant_and_type_conversion()) {
        return 3;
    }

    if (!test_preserve_stack()) {
        return 4;
    }

    return 0;  // success
}
