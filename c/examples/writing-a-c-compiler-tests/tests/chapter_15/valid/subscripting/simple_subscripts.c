/* Test out simple cases involving constant indices and one-dimensional arrays */

#if defined SUPPRESS_WARNINGS && !defined __clang__
#pragma GCC diagnostic ignored "-Wtautological-compare"
#endif

int integer_types(unsigned *arr, unsigned expected) {
    // make sure our index can be any integer type
    unsigned val1 = arr[5];
    unsigned val2 = arr[5u];
    unsigned val3 = arr[5l];
    unsigned val4 = arr[5ul];
    if (val1 != expected) {
        return 1;
    }

    if (val2 != expected) {
        return 2;
    }

    if (val3 != expected) {
        return 3;
    }

    if (val4 != expected) {
        return 4;
    }
    return 0;
}

// x[i] == i[x] - doesn't matter which is the index
int reverse_subscript(long *arr, long expected)  {
    if (arr[3] != expected) {
        return 5;
    }

    if (3[arr] != expected) {
        return 6;
    }

    // taking address of both expression should yield same address
    if (&3[arr] != &arr[3]) {
        return 7;
    }

    return 0;
}

// subscript a static array
static double static_array[3] = {0.1, 0.2, 0.3};

int subscript_static(void) {
    if (static_array[0] != 0.1) {
        return 8;
    }
    if (static_array[1] != 0.2) {
        return 9;
    }
    if (static_array[2] != 0.3) {
        return 10;
    }
    return 0;
}

// update an array element using subscripting
// expected is new value of arr[10] after update
int update_element(int *arr, int expected) {
    arr[10] = arr[10] * 2;

    if (arr[10] != expected) {
        return 11;
    }

    return 0;
}

// update an array element with static storage duration using subscripting
int *increment_static_element(void) {
    static int arr[4];
    arr[3] = arr[3] + 1;
    return arr;
}

int check_increment_static_element(void) {
    // increment static arr and get a pointer to it
    int *arr1 = increment_static_element();

    // last element should be 1, all others should be 0
    if (arr1[3] != 1) {
        return 12;
    }

    if (arr1[0] || arr1[1] || arr1[2]) {
        return 13;
    }

    // call function again to increment last element again
    int *arr2 = increment_static_element();

    if (arr1 != arr2) {
        return 14;
    }

    if (arr1[3] != 2) {
        return 15;
    }

    return 0;
}

int main(void) {
    unsigned int unsigned_arr[6] = {0, 0, 0, 0, 0, 7u};
    // unsigned_arr[5] == 7
    int check = integer_types(unsigned_arr, 7u);
    if (check) {
        return check;
    }

    long int long_arr[4] = {100, 102, 104, 106};
    // long_arr[3] == 106
    check = reverse_subscript(long_arr, 106);
    if (check) {
        return check;
    }

    check = subscript_static();
    if (check) {
        return check;
    }

    int int_arr[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15};
    check = update_element(int_arr, 30);
    if (check) {
        return check;
    }

    check = check_increment_static_element();
    if (check) {
        return check;
    }

    return 0;

}