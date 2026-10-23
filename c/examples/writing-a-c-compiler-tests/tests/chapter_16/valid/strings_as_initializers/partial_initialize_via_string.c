/* Test that when we initialize an array from a string literal,
 * we zero out elements that aren't explicitly initialized.
 * */

static char static_arr[5] = "hi";
int test_static(void) {
    return (static_arr[0] == 'h' && static_arr[1] == 'i' &&
            !(static_arr[2] || static_arr[3] || static_arr[4]));
}

static signed char nested_static_arr[3][4] = {
    "", "bc"};  // empty string just initializes to null byte
int test_static_nested(void) {
    for (int i = 0; i < 3; i = i + 1)
        for (int j = 0; j < 4; j = j + 1) {
            signed char c = nested_static_arr[i][j];

            // nested_static_arr[1][0] and nested_static_arr[1][1]
            // have values from initializer; all other elements are 0
            signed char expected = 0;
            if (i == 1 && j == 0) {
                expected = 'b';
            } else if (i == 1 && j == 1) {
                expected = 'c';
            }

            if (c != expected) {
                return 0;  // failure
            }
        }

    return 1;  // success
}

int test_automatic(void) {
    unsigned char aut[4] = "ab";
    // first two elements have values from initializer, last two are 0
    return (aut[0] == 'a' && aut[1] == 'b' && !(aut[2] || aut[3]));
}

int test_automatic_nested(void) {
    signed char nested_auto[2][2][4] = {{"foo"}, {"x", "yz"}};
    for (int i = 0; i < 2; i = i + 1) {
        for (int j = 0; j < 2; j = j + 1) {
            for (int k = 0; k < 4; k = k + 1) {
                signed char c = nested_auto[i][j][k];
                signed char expected = 0;
                if (i == 0 && j == 0) {
                    if (k == 0) {
                        expected = 'f';
                    } else if (k == 1 || k == 2) {
                        expected = 'o';
                    }
                } else if (i == 1 && j == 0 && k == 0) {
                    expected = 'x';
                } else if (i == 1 && j == 1 && k == 0) {
                    expected = 'y';
                } else if (i == 1 && j == 1 && k == 1) {
                    expected = 'z';
                }

                if (c != expected) {
                    return 0;  // failure
                }
            }
        }
    }
    return 1;  // success
}

int main(void) {
    if (!test_static()) {
        return 1;
    }

    if (!test_static_nested()) {
        return 2;
    }

    if (!test_automatic()) {
        return 3;
    }

    if (!test_automatic_nested()) {
        return 4;
    }

    return 0;
}