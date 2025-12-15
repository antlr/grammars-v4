/* Test initializing one-dimensional arrays with static storage duration */

// fully initialized
double double_arr[3] = {1.0, 2.0, 3.0};

int check_double_arr(double *arr) {
    if (arr[0] != 1.0) {
        return 1;
    }

    if (arr[1] != 2.0) {
        return 2;
    }

    if (arr[2] != 3.0) {
        return 3;
    }

    return 0;
}

// partly initialized
unsigned uint_arr[5] = {
    1u,
    0u,
    2147497230u,
};

int check_uint_arr(unsigned *arr) {
    if (arr[0] != 1u) {
        return 4;
    }

    if (arr[1]) {
        return 5;
    }
    if (arr[2] != 2147497230u) {
        return 6;
    }

    if (arr[3] || arr[4]) {
        return 7;
    }

    return 0;
}

// uninitialized; should be all zeros
long long_arr[1000];

int check_long_arr(long *arr) {
    for (int i = 0; i < 1000; i = i + 1) {
        if (arr[i]) {
            return 8;
        }
    }
    return 0;
}

// initialized w/ values of different types
unsigned long ulong_arr[4] = {
    100.0, 11, 12345l, 4294967295U
};

int check_ulong_arr(unsigned long *arr) {
    if (arr[0] != 100ul) {
        return 9;
    }

    if (arr[1] != 11ul) {
        return 10;
    }

    if (arr[2] != 12345ul) {
        return 11;
    }

    if (arr[3] != 4294967295Ul) {
        return 12;
    }
    return 0;
}

int test_global(void) {
    int check = check_double_arr(double_arr);
    if (check) {
        return check;
    }

    check = check_uint_arr(uint_arr);
    if (check) {
        return check;
    }
    check = check_long_arr(long_arr);
    if (check) {
        return check;
    }
    check = check_ulong_arr(ulong_arr);
    if (check) {
        return check;
    }
    return 0;
}

// equivalent static local arrays
int test_local(void) {

    // fully initialized
    double local_double_arr[3] = {1.0, 2.0, 3.0};
    // partly initialized
    static unsigned local_uint_arr[5] = {
        1u,
        0u, // truncated to 0
        2147497230u,
    };

    // uninitialized
    static long local_long_arr[1000];

    // initialized w/ values of different types
    static unsigned long local_ulong_arr[4] = {
        100.0, 11, 12345l, 4294967295U
    };

    // validate
    int check = check_double_arr(local_double_arr);
    if (check) {
        return 100 + check;
    }

    check = check_uint_arr(local_uint_arr);
    if (check) {
        return 100 + check;
    }
    check = check_long_arr(local_long_arr);
    if (check) {
        return 100 + check;
    }
    check = check_ulong_arr(local_ulong_arr);
    if (check) {
        return 100 + check;
    }
    return 0;
}

int main(void) {
    int check = test_global();
    if (check) {
        return check;
    }
    return test_local();
}
