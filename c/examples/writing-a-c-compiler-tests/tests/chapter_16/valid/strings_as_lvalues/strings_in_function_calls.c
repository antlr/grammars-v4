/* Test that we can use strings literals as function arguments/return values */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

unsigned long strlen(char *s);

char *return_string(void) {
    // constant strings have static storage duration,
    // so this will persist after the function call;
    return "I'm a string!";
}

int pass_string_args(char *s1, char *s2) {
    // neither should be a null pointer
    if (s1 == 0 || s2 == 0) {
        return 0;
    }

    if (strlen(s1) != 45) {
        return 0;
    }

    if (s1[41] != 'd' || s1[42] != 'o' || s1[43] != 'g') {
        return 0;
    }

    // s2 is an empty string so first byte should be null
    if (s2[0]) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    char *ptr = 0;
    // call return_string and inspect results
    ptr = return_string();
    if (!ptr)
        return 1;

    if (ptr[0] != 'I' || ptr[1] != '\'' || ptr[13]) {
        return 2;
    }

    // pass strings as function arguments
    if (!pass_string_args("The quick brown fox jumped over the lazy dog.",
                          "")) {
        return 3;
    }

    return 0;

    char *ptr2;
    ptr2 = 1 ? ptr + 2 : ptr + 4;
    return *ptr2 == 'm';
}