/* Test subscript expressions where both operands are complex sub-expressions,
 * not just variables and constants. This test program only includes 1D arrays. */

// use a side-effecting statement as an index
int assign_in_index(int idx) {
    int arr[3] = {1, 2, 3};
    int val = arr[idx = idx + 2];
    if (idx != 1) {
        return 1;
    }

    if (val != 2) {
        return 2;
    }

    return 0;
}

// helper function for funcall_in_index
int static_index(void) {
    static int index = 0;
    int retval = index;
    index = index + 1;
    return retval;
}

// use a side-effecting function call as an index
int funcall_in_index(void) {
    int arr[3] = {1, 2, 3};
    int v1 = arr[static_index()];
    int v2 = arr[static_index()];
    if (v1 != 1) {
        return 3;
    }
    if (v2 != 2) {
        return 4;
    }

    return 0;
}

// use result of another subscript expression as index
int subscript_inception(long *arr, int *a, int b){
    return arr[a[b]];
}

int check_subscript_inception(void) {
    long arr[4] = {4, 3, 2, 1};
    int indices[2] = {1, 2};
    if (subscript_inception(arr, indices, 1) != 2) {
        return 5;
    }

    if (subscript_inception(arr, indices, 0) != 3) {
        return 6;
    }

    return 0;
}

// use result of function call as pointer
int *get_array(void) {
    static int arr[3];
    return arr;
}

int subscript_function_result(void){
    get_array()[2] = 1;
    if (get_array()[2] != 1) {
        return 7;
    }

    return 0;
}

int negate_subscript(int *arr, int idx, int expected) {
    if (arr[-idx] != expected) {
        return 8;
    }

    return 0;
}

int main(void) {
    int check = assign_in_index(-1);
    if (check) {
        return check;
    }

    check = funcall_in_index();
    if (check) {
        return check;
    }

    check = check_subscript_inception();
    if (check) {
        return check;
    }

    check = subscript_function_result();
    if (check) {
        return check;
    }

    int arr[3] = {0, 1, 2};
    check = negate_subscript(arr + 2, 2, 0);
    if (check) {
        return check;
    }
    return 0;
}