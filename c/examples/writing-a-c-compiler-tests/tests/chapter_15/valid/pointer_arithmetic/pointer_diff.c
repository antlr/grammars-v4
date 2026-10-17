/* Test subtracting two pointers to find the number of elements between them */


/* subtract two pointers into a 1D array of ints */
int get_ptr_diff(int *ptr1, int *ptr2) {
    return (ptr2 - ptr1);
}

/* subtract two pointers into array of longs */
int get_long_ptr_diff(long *ptr1, long *ptr2) {
    return (ptr2 - ptr1);
}

/* subtract pointers to two elements in a multi-dimensional array */
int get_multidim_ptr_diff(double (*ptr1)[3][5], double (*ptr2)[3][5]) {
    return (ptr2 - ptr1);
}

/* subtract pointers into a multi-dimensional array again, but at different levels of nesting */
int get_multidim_ptr_diff_2(double (*ptr1)[5], double (*ptr2)[5]) {
    return (ptr2 - ptr1);
}

int main(void) {
    int arr[5] = {5, 4, 3, 2, 1};
    int *end_of_array = arr + 5;

    if (get_ptr_diff(arr, end_of_array) != 5) {
        return 1;
    }

    long long_arr[8];

    if (get_long_ptr_diff(long_arr + 3, long_arr) != -3) {
        return 2;
    }

    // test subtracting multi-dimensional pointers;
    // also make sure we can handle pointers into array with static storage duration
    static double multidim[6][7][3][5];

    if (get_multidim_ptr_diff(multidim[2] + 1, multidim[2] + 4) != 3) {
        return 3;
    }

    if (get_multidim_ptr_diff_2(multidim[2][2] + 2, multidim[2][2]) != -2) {
        return 4;
    }

    return 0;
}