// Postfix ++/-- and subscript have higher precedence than prefix ++/--
int idx = 3;
int main(void) {
    int arr[5] = {1, 2, 3, 4, 5};
    int *ptr = arr + 1;
    // 1. evaluate ptr--; this yields a pointer to arr[1], makes ptr point to arr[0]
    // 2. evaluate subscript operation, yielding lval at arr[4]
    // 3. increment lval at arr[4] (to 6) and return incremented value
    int result = ++ptr--[idx];

    // check result
    if (result != 6) {
        return 1; // fail
    }

    // check side effect of decrementing pointer
    if (*ptr != 1) {
        return 2; // fail
    }

    // check side effect of decrementing pointer (a different way)
    if (ptr != arr) {
        return 3; // fail
    }

    // make sure postfix ++ is higher precedence than dereference (*) operator
    if (*ptr++ != 1) {
        return 4; // fail
    }

    // check side effect of decrementing pointer
    if (*ptr != 2) {
        return 5;
    }

    // first four elements of arr have value as before same
    for (int i = 0; i < 4; i++) {
        if (arr[i] != i + 1) {
            return 6; // fail
        }
    }

    // check side effect of incrementing last element
    if (arr[4] != 6) {
        return 7; // fail
    }

    return 0;
}
