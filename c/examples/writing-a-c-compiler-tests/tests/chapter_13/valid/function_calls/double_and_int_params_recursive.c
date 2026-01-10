

/* A recursive function in which both double and integer parameters
 * are passed in registers and on the stack */
int fun(int i1, double d1, int i2, double d2, int i3, double d3,
        int i4, double d4, int i5, double d5, int i6, double d6,
        int i7, double d7, int i8, double d8, int i9, double d9) {


    if (i1 != d9) {
        /* make two recursive calls that bring these values closer together:
         * 1. increment i1 and all ints: */
        int call1 = fun(i1 + 1, d1, i2 + 1, d2, i3 + 1, d3, i4 + 1, d4, i5 + 1, d5, i6 + 1, d6, i7 + 1, d7, i8 + 1, d8, i9 + 1, d9);

        /* 2. decrement d9 and all doubles */
        int call2 = fun(i1, d1 - 1, i2, d2 - 1, i3, d3 - 1, i4, d4 - 1, i5, d5 - 1, i6, d6 - 1, i7, d7 - 1, i8, d8 - 1, i9, d9 - 1);

        /* Make sure both calls succeeded; non-zero result indicates a problem */
        if (call1) {
            return call1;
        }

        if (call2) {
            return call2;
        }

    }

    // make sure all arguments have expected value; value of each arg relative to i1 (for ints)
    // or d9 (for doubles) should stay the same. this ensures that we preserve value of function parameters
    // even across other function calls
    if (i2 != i1 + 2) {
        return 2;
    }
    if (i3 != i1 + 4) {
        return 3;
    }
    if (i4 != i1 + 6) {
        return 4;
    }
    if (i5 != i1 + 8) {
        return 5;
    }
    if (i6 != i1 + 10) {
        return 6;
    }
    if (i7 != i1 + 12) {
        return 7;
    }
    if (i8 != i1 + 14) {
        return 8;
    }
    if (i9 != i1 + 16) {
        return 9;
    }
    if (d1 != d9 - 16) {
        return  11;
    }
    if (d2 != d9 - 14) {
        return  12;
    }
    if (d3 != d9 - 12) {
        return  13;
    }
    if (d4 != d9 - 10) {
        return  14;
    }
    if (d5 != d9 - 8) {
        return  15;
    }
    if (d6 != d9 - 6) {
        return  16;
    }
    if (d7 != d9 - 4) {
        return  17;
    }
    if (d8 != d9 - 2) {
        return  18;
    }

    return 0;
}

int main(void) {
    return fun(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0, 11, 12.0, 13, 14.0, 15, 16.0, 17, 18.0);
}