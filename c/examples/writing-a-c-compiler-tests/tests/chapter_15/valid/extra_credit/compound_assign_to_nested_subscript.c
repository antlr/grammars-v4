// test compound assignment where LHS is nested subscripted expression

long long_nested_arr[2][3] = {{1, 2, 3}, {4, 5, 6}};
double dbl_nested_arr[3][2] = {{100.0, 101.0}, {102.0, 103.0}, {104.0, 105.0}};
unsigned unsigned_index = 10;

int main(void) {
    // nested long array
    if ((long_nested_arr[1][unsigned_index - 8] *= -1) != -6) {
        return 1;  // fail
    }
    if (long_nested_arr[1][2] != -6) {
        return 2;  // fail
    }

    // make sure other five elements are unchanged
    for (int i = 0; i < 2; i += 1) {
        for (int j = 0; j < 3; j += 1) {
            if (i == 1 && j == 2) {
                // this is the one we just checked
                break;
            }
            long expected = i * 3 + j + 1;
            if (long_nested_arr[i][j] != expected) {
                return 3;  // fail
            }
        }
    }

    // another nested array
    if ((dbl_nested_arr[1][1] += 100.0) != 203.0) {
        return 4;  // fail
    }

    // make sure the other elements of dbl_nested_arr are unchanged
    for (int i = 0; i < 3; i += 1) {
        for (int j = 0; j < 2; j += 1) {
            if (i == 1 && j == 1) {
                // we already validated this one
                continue;
            }
            int expected = 100 + i * 2 + j;
            if (dbl_nested_arr[i][j] != expected) {
                return 5;  // fail
            }
        }
    }

    return 0;  // success
}