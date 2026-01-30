#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wchar-subscripts"
#endif
/* Test that we can use chars in the same expressions as other integers,
 * including arithmetic, pointer arithmetic, comparisons, and logical
 * operations
 * */

// basic arithmetic (these chars will be promoted to ints,
// but this test case doesn't focus on that; we handle that in a different test)
int add_chars(char c1, char c2) {
    return c1 + c2;
}

int divide_chars(unsigned char c1, unsigned char c2) {
    return c1 / c2;
}

// compare chars
int le(char c1, char c2) {
    return c1 <= c2;
}

// use chars in subscript expressions
int subscript_char(int *ptr, char idx){
    return ptr[idx];
}

// use chars in pointer arithmetic
int *sub_char_from_pointer(int *ptr, signed char idx) {
    return ptr - idx;
}

// use char in logical operations
int and_char(signed char c1, int i) {
    return c1 && i;
}

int or_char(signed char c1, unsigned char c2) {
    return c1 || c2;
}

// declare/use char in controlling expression
int test_for_loop_char(void) {
    int counter = 0;
    for (signed char s = 127; s > 0; s = s - 1) {
        counter = counter + 1;
    }
    return (counter == 127);
}

int main(void) {

    // arithmetic
    char c1 = 8;
    char c2 = 4;
    if (add_chars(c1, c2) != 12)  {
        return 1;
    }

    unsigned char uc1 = 250;
    unsigned char uc2 = 25;
    if (divide_chars(uc1, uc2) != 10) {
        return 2;
    }

    // comparisons
    if (le(c1, c2)) {
        return 3;
    }

    if (!le(c2, c2)) {
        return 4;
    }

    // pointer arithmetic
    int arr[4] = {11, 12, 13, 14};
    char idx = 2;
    if (subscript_char(arr, idx) != 13) {
        return 5;
    }

    signed char offset = 1;
    if (sub_char_from_pointer(arr + 1, offset) != arr) {
        return 6;
    }

    // logical operations
    signed char zero = 0;
    if (zero) {
        return 7;
    }

    if (and_char(zero, 12)) {
        return 8;
    }

    uc2 = 0;
    if (or_char(zero, uc2)) {
        return 9;
    }

    if (!test_for_loop_char()) {
        return 10;
    }

    return 0;
}