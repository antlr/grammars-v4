/* Test initializing multi-dimensional arrays with static storage duration */

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma GCC diagnostic ignored "-Wliteral-conversion"
#endif

// fully initialized
double double_arr[2][2] = {{1.1, 2.2}, {3.3, 4.4}};

int check_double_arr(double (*arr)[2]) {
    if (arr[0][0] != 1.1) {
        return 1;
    }

    if (arr[0][1] != 2.2) {
        return 2;
    }

    if (arr[1][0] != 3.3) {
        return 3;
    }

    if (arr[1][1] != 4.4) {
        return 4;
    }

    return 0;
}

// uninitialized; should be all zeros
long long_arr[30][50][40];

int check_long_arr(long (*arr)[50][40]) {
    for (int i = 0; i < 30; i = i + 1) {
        for (int j = 0; j < 50; j = j + 1) {
            for (int k = 0; k < 40; k = k + 1) {
                if (arr[i][j][k]) {
                    return 5;
                }
            }
        }
    }

    return 0;
}

// partially initialized using values of different types

unsigned long ulong_arr[4][6][2] = {
    {{
         1000.3,
     }, // truncated to 1000
     {12u}},
    {{2}}};

int check_ulong_arr(unsigned long (*arr)[6][2]) {
    for (int i = 0; i < 4; i = i + 1) {
        for (int j = 0; j < 6; j = j + 1) {
            for (int k = 0; k < 2; k = k + 1) {
                int val = arr[i][j][k];
                if (i == 0 && j == 0 && k == 0) {
                    if (val != 1000ul) {
                        return 6;
                    }
                } else if (i == 0 && j == 1 && k == 0) {
                    if (val != 12ul) {
                        return 7;
                    }
                } else if (i == 1 && j == 0 && k == 0) {
                    if (val != 2ul) {
                        return 8;
                    }
                } else {
                    // not explicitly initialized, should be 0
                    if (val) {
                        return 9;
                    }
                }
            }
        }
    }

    return 0;
}

// validate all the global arrays
int test_global(void) {
    int check = check_double_arr(double_arr);
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

    static double local_double_arr[2][2] = {{1.1, 2.2}, {3.3, 4.4}};

    int check = check_double_arr(local_double_arr);
    if (check) {
        return 100 + check;
    }

    static long local_long_arr[30][50][40];
    check = check_long_arr(local_long_arr);
    if (check) {
        return 100 + check;
    }

    static unsigned long local_ulong_arr[4][6][2] = {
        {{
            1000.3,
        }, // truncated to 1000
        {12u}},
        {{2}}};
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