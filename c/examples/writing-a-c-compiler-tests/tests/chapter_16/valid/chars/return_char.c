/* Test that we can call functions with return values of character type,
 * and that accessing these return values doesn't clobber other things on the
 * stack
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

char return_char(void) {
    return 5369233654l;  // this will be truncated to -10
}

signed char return_schar(void) {
    return 5369233654l;  // this will be truncated to -10
}

unsigned char return_uchar(void) {
    return 5369233654l;  // this will be truncated to 246
}

int main(void) {
    char char_array[3] = {121, -122, -3};
    char retval_c = return_char();
    char char_array2[3] = {-5, 88, -100};
    signed char retval_sc = return_schar();
    char char_array3[3] = {10, 11, 12};
    unsigned char retval_uc = return_uchar();
    char char_array4[2] = {-5, -6};

    // make sure we got the right return values and didn't overwrite
    // other arrays on the stack
    if (char_array[0] != 121 || char_array[1] != -122 || char_array[2] != -3) {
        return 1;
    }

    if (retval_c != -10) {
        return 2;
    }
    if (char_array2[0] != -5 || char_array2[1] != 88 ||
        char_array2[2] != -100) {
        return 3;
    }

    if (retval_sc != -10) {
        return 4;
    }
    if (char_array3[0] != 10 || char_array3[1] != 11 || char_array3[2] != 12) {
        return 5;
    }
    if (retval_uc != 246) {
        return 6;
    }
    if (char_array4[0] != -5 || char_array4[1] != -6) {
        return 7;
    }
    return 0;
}