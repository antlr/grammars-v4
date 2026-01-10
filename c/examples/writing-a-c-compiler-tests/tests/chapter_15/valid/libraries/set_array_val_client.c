/* Make sure we can pass pointers to array elements,
 * including nested array elements, as function arguments. */

int set_nth_element(double *arr, int idx);
int set_nested_element(int (*arr)[2], int i, int j);

int main(void) {

    // pass a 1D array as a function argument
    double arr[5] = {0.0, 0.0, 0.0, 0.0, 0.0};

    // if this is non-zero, value of arr passed to set_nth_element was wrong
    int check = set_nth_element(arr, 4);
    if (check) {
        return check;
    }

    // make sure updated values are correct
    for (int i = 0; i < 4; i = i + 1) {
        if (arr[i] != 0) {
            return 2;
        }
    }
    if (arr[4] != 8)
        return 3;

    // now try a 2D array
    int nested_arr[3][2] = {{-10, -9}, {-8, -7}, {-6, -5}};

    // if this is non-zero, value of arr passed to set_nested_element was wrong
    check = set_nested_element(nested_arr, 2, 1);
    if (check) {
        return check;
    }

    // make sure updated values are correct
    for (int i = 0; i < 3; i = i + 1) {
        for (int j = 0; j < 2; j = j + 1) {

            if (i == 2 && j == 1) {
                // this is the element we just updated
                if (nested_arr[i][j] != 10) {
                    return 5;
                }
            } else {
                // value shoudl be the same as before
                int expected = -10 + 2 * i + j;
                if (nested_arr[i][j] != expected) {
                    return 6;
                }
            }
        }
    }

    return 0;
}