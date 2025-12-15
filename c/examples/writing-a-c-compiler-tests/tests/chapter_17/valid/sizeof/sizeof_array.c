/* Test that arrays don't decay to pointers
 * when they're the operands of sizeof expression */

#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wsizeof-array-argument"
#endif

unsigned long sizeof_adjusted_param(int arr[3]) {
    // this should return the size of arr's _adjusted_ type,
    // so it should return 8 (the size of a pointer) instead of 12
    return sizeof arr;
}

int main(void) {
    // flat array
    int arr[3];
    if (sizeof arr != 12) {
        return 1;
    }

    static long nested_arr[4][5];

    // arr[2] has type long[5], so its size is 8 * 5 = 40
    if (sizeof nested_arr[2] != 40) {
        return 2;
    }

    // string literals also don't decay to pointers in sizeof expressions
    if (sizeof "Hello, World!" != 14) {
        return 3;
    }

    // parameters declared with array type are adjusted to pointers,
    // and sizeof reflects this
    if (sizeof_adjusted_param(arr) != 8) {
        return 4;
    }

    return 0;
}