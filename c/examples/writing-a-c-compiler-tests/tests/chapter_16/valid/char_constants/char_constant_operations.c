/* Test that we treat character constants like integers */

// use character constants to initialize static variables of any arithmetic type
double d = '\\'; // ASCII value 92

int main(void) {

    if (d != 92.0) {
        return 1;
    }

    // You can use character constants to specify array dimensions
    // and initialize array elements of arithmetic type
    unsigned long array['\n'] = {1, 2, 'a', '\b', 3, 4, 5, '!', '%', '~'};

    if (array[2] != 97) {
        return 2;
    }

    if (array[3] != 8) {
        return 3;
    }

    if (array[7] != 33) {
        return 4;
    }

    if (array[8] != 37) {
        return 5;
    }

    if (array[9] != 126) {
        return 6;
    }

    // make sure array has the right length (20) by initializing a pointer to its address
    // if we've got the wrong size this will be a type error
    unsigned long (*array_ptr)[10] = &array;

    if (array_ptr[0][9] != '~') {
        return 7;
    }

    // You can use character constants in subscript expressions
    int i = array['\a']; // ASCII value of \a is 7
    if (i != 33) {
        return 8;
    }

    // You can use character constants in arithmetic expressions
    double d = 10 % '\a' + 4.0 * '_' - ~'@'; // 10 % 7 + 4.0 * 95 - ~64

    if (d != 448.0) {
        return 9;
    }
    return 0;
}