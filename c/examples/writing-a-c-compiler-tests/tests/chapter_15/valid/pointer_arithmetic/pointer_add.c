/* Test pointer addition and subtraction to specify array indices
 * (but not subtracting two pointers to get the distance between them)
 * */

/* Addition */

/* basic pointer addition */
int test_add_constant_to_pointer(void) {
    long long_arr[12] = {0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 13};
    long *ptr = long_arr + 10;
    return *ptr == 13;
}

/* add negative index to pointer */
int test_add_negative_index(void) {
    unsigned unsigned_arr[12] = {0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 42};
    unsigned *end_ptr = unsigned_arr + 12;

    unsigned *ptr = end_ptr + -10;
    return *ptr == 2;
}

/* it doesn't matter whether we add pointer to int or vice versa */
int test_add_pointer_to_int(void) {
    int int_arr[5] = {0, 98, 99};
    int *ptr1 = int_arr + 2;
    int *ptr2 = 2 + int_arr;

    return (ptr1 == ptr2 && *ptr2 == 99);
}

/* array index can be any integer type, not just int */
int test_add_different_index_types(void) {
    double double_arr[11] = {0, 0, 0, 0, 0, 6.0};

    // four equivalent expresssions that should produce the same pointer
    double *ptr1 = double_arr + 5;
    double *ptr2 = double_arr + 5l;
    double *ptr3 = double_arr + 5u;
    double *ptr4 = double_arr + 5ul;

    return (ptr1 == ptr2 && ptr1 == ptr3 && ptr1 == ptr4 && *ptr4 == 6.0);
}

/* pointer addition where pointer and index are both complex expressions */
int test_add_complex_expressions(void) {
    // use some static variables and function calls so operands
    // won't be constant-folded away in Part III
    static int flag;  // 0
    int i = -2;
    int *small_int_ptr = &i;
    extern int return_one(void);
    extern int *get_elem1_ptr(int *arr);
    extern int *get_elem2_ptr(int *arr);
    static int arr[4] = {1, 2, 3, 4};
    // ptr = 1 + -2 + (0 ? (arr + 1) : (arr + 2))
    //  => -1 + (arr + 2)
    //  => arr + 1
    int *ptr = return_one() + (*small_int_ptr) +
               (flag ? get_elem1_ptr(arr) : get_elem2_ptr(arr));
    return (ptr == arr + 1 && *ptr == 2);
}

// define our helper functions for the test case above
int return_one(void) {
    return 1;
}

int *get_elem1_ptr(int *arr) {
    return arr + 1;
}

int *get_elem2_ptr(int *arr) {
    return arr + 2;
}

/* add pointers to rows in a multi-dimensional array */
int test_add_multi_dimensional(void) {
    static int index = 2;
    int nested_arr[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    int(*row_pointer)[3] = nested_arr + index;
    return **row_pointer == 7;
}

/* add pointers to scalar elements in a multi-dimensional array */
int test_add_to_subarray_pointer(void) {
    static int index = 2;
    int nested_arr[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    // pointer to nested_arr[1]
    int *row1 = *(nested_arr + 1);

    // pointer to nested_arr[1][2]
    int *elem_ptr = row1 + index;
    return *elem_ptr == 6;
}

/* Subtraction */

/* Subtract a variable from a pointer */
int test_subtract_from_pointer(void) {
    long long_arr[5] = {10, 9, 8, 7, 6};
    long *one_past_the_end = long_arr + 5;
    static int index = 3;
    long *subtraction_result = one_past_the_end - index;
    return *subtraction_result == 8;
}

/* Subtract negative index from pointer */
int test_subtract_negative_index(void) {
    unsigned arr[5] = {100, 101, 102, 103, 104};
    unsigned *ptr = arr - (-3);
    return *ptr == 103;
}

/* array index can be any integer type, not just int */
int test_subtract_different_index_types(void) {
    double double_arr[11] = {0, 0, 0, 0, 0, 0, 6.0};
    double *end_ptr = double_arr + 11;

    // four equivalent expresssions that should produce the same pointer
    double *ptr1 = end_ptr - 5;
    double *ptr2 = end_ptr - 5l;
    double *ptr3 = end_ptr - 5u;
    double *ptr4 = end_ptr - 5ul;
    return (ptr1 == ptr2 && ptr1 == ptr3 && ptr1 == ptr4 && *ptr4 == 6.0);
}

/* index and pointer can both be arbitrary expressions, not just constants and
 * variables */
int test_subtract_complex_expressions(void) {
    static int flag = 1;
    static int four = 4;
    static int arr[4] = {1, 2, 3, 4};
    // reuse get_elem1_ptr and get_elem2_ptr funcionts we defined earlier
    // ptr = (1 ? (arr + 1) : (arr + 2)) - (4/-2)
    //  => (arr + 1) - -2
    //  => arr + 3
    int *ptr = (flag ? get_elem1_ptr(arr) : get_elem2_ptr(arr)) - (four / -2);
    return (*ptr == 4);
}

/* subtract pointers to rows in a multi-dimensional array */
int test_subtract_multi_dimensional(void) {
    static int index = 1;
    int nested_arr[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    int(*last_row_pointer)[3] = nested_arr + 2;
    int(*row_pointer)[3] = last_row_pointer - index;
    return (**row_pointer == 4);
}

int main(void) {
    /* Addition */
    if (!test_add_constant_to_pointer()) {
        return 1;
    }

    if (!test_add_negative_index()) {
        return 2;
    }

    if (!test_add_pointer_to_int()) {
        return 3;
    }

    if (!test_add_different_index_types()) {
        return 4;
    }

    if (!test_add_complex_expressions()) {
        return 5;
    }

    if (!test_add_multi_dimensional()) {
        return 6;
    }

    if (!test_add_to_subarray_pointer()) {
        return 7;
    }

    /* Subtraction */
    if (!test_subtract_from_pointer()) {
        return 8;
    }

    if (!test_subtract_negative_index()) {
        return 9;
    }

    if (!test_subtract_different_index_types()) {
        return 10;
    }

    if (!test_subtract_complex_expressions()) {
        return 11;
    }

    return 0;
}