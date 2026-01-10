/* make sure we can use a mix of string literals and compound initializers to
 * initialize a single nested array */

// array wih static storage duration
signed char static_array[3][4] = {{'a', 'b', 'c', 'd'}, "efgh", "ijk"};

int main(void) {
    // array with automatic storage duration
    unsigned char auto_array[2][3] = {"lmn", {'o', 'p'}};

    // validate static array
    for (int i = 0; i < 3; i = i + 1)
        for (int j = 0; j < 4; j = j + 1)
            if (static_array[i][j] != "abcdefghijk"[i * 4 + j])
                return 1;

    // validate automatic array
    for (int i = 0; i < 2; i = i + 1)
        for (int j = 0; j < 3; j = j + 1)
            if (auto_array[i][j] != "lmnop"[i * 3 + j])
                return 2;

    return 0;
}