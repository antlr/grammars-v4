/* Make sure we can define/call functions that return pointers to arrays */
long (*return_row(long (*arr)[3][4], int idx))[4];

int main(void) {
    long nested_array[2][3][4] = {
        {{0}},
        {{-12, -13, -14, -15}, {-16}}
    };

    long (*row_pointer)[4] = return_row(nested_array, 1);

    // make sure values are correctly
    for (int i = 0; i < 3; i = i + 1) {
        for (int j = 0; j < 4; j = j + 1) {
            if (row_pointer[i][j] != nested_array[1][i][j]) {
                return 1;
            }
        }
    }

    // make sure that when we update the array through one pointer,
    // it's visible in the other

    row_pointer[2][1] = 100;
    if (nested_array[1][2][1] != 100) {
        return 2;
    }

    return 0;
}