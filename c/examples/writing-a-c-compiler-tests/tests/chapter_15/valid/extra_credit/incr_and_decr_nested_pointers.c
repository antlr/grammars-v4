// Array arithmetic with prefix and postfix ++/--
int main(void) {
    long arr[2][3][4] = {
        {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}},
        {{13, 14, 15, 16}, {17, 18, 19, 20}, {21, 22, 23, 24}}};

    // pointer to outermost level
    long (*outer_ptr)[3][4] = arr + 1;
    if (outer_ptr-- != &arr[1]) {
        return 1; // fail
    }
    if (outer_ptr[0][1] != arr[0][1]) {
        return 2; // fail
    }

    if ((++outer_ptr)[0][2][3] != 24) {
        return 3; // fail
    }
    if (outer_ptr[0][2][3] != 24) {
        return 4; // fail
    }

    // pointer to next level in
    long (*inner_ptr)[4] = arr[0] + 1;
    if (inner_ptr++[0][2] != 7) {
        return 5; // fail
    }

    if (inner_ptr[0][2] != 11) {
        return 6; // fail
    }

    if ((--inner_ptr)[0][1] != 6) {
        return 7; // fail
    }

    // pointer to scalar elements
    long *scalar_ptr = arr[1][2];
    if (scalar_ptr--[2] != 23) {
        return 8; // fail
    }
    if (scalar_ptr[2] != 22) {
        return 9; // fail
    }

    return 0;  // success
}