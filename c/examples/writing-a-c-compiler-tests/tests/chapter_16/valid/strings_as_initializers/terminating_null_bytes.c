/* When we initialize an array from a string literal,
 * make sure we include the null byte if the array has space for it,
 * and exclude it otherwise
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);  // standard library

// test cases where we include null byte
int test_flat_static_with_null_byte(void) {
    static unsigned char flat[4] = "dog";
    return (flat[0] == 'd' && flat[1] == 'o' && flat[2] == 'g' && flat[3] == 0);
}

int test_nested_static_with_null_byte(void) {
    static char nested[2][4] = {"yes", "yup"};
    return (nested[0][0] == 'y' && nested[0][1] == 'e' && nested[0][2] == 's' &&
            nested[0][3] == 0 && nested[1][0] == 'y' && nested[1][1] == 'u' &&
            nested[1][2] == 'p' && nested[1][3] == 0);
}

int test_flat_auto_with_null_byte(void) {
    char flat_auto[2] = "x";
    return (flat_auto[0] == 'x' && flat_auto[1] == 0);
}

int test_nested_auto_with_null_byte(void) {
    char nested_auto[2][2][2] = {{"a", "b"}, {"c", "d"}};

    return (nested_auto[0][0][0] == 'a' && nested_auto[0][0][1] == 0 &&
            nested_auto[0][1][0] == 'b' && nested_auto[0][1][1] == 0 &&
            nested_auto[1][0][0] == 'c' && nested_auto[1][0][1] == 0 &&
            nested_auto[1][1][0] == 'd' && nested_auto[1][1][1] == 0);
}

// test cases where we omit null byte

int test_flat_static_without_null_byte(void) {
    // it shouldn't corrupt anything if we add a null byte here,
    // so this is just to make sure it type checks
    static char letters[4] = "abcd";
    return letters[0] == 'a' && letters[1] == 'b' && letters[2] == 'c' &&
           letters[3] == 'd';
}

// we can't fit a null byte at the end of 'yes'
char nested[3][3] = {"yes", "no", "ok"};
int test_nested_static_without_null_byte(void) {
    char *whole_array = (char *)nested;
    char *word1 = (char *)nested[0];
    char *word2 = (char *)nested[1];
    char *word3 = (char *)nested[2];
    // all strcmp calls should return 0
    return !(strcmp(whole_array, "yesno") || strcmp(word1, "yesno") ||
             strcmp(word2, "no") || strcmp(word3, "ok"));
}

int test_flat_auto_without_null_byte(void) {
    int x = -1;
    // make sure we don't add a null byte here; it would corrupt neighboring
    // ints
    char letters[4] = "abcd";
    int y = -1;
    return (x == -1 && y == -1 && letters[0] == 'a' && letters[1] == 'b' &&
            letters[2] == 'c' && letters[3] == 'd');
}

// identical to test_static_nested_without_null_byte(, but array has automatic
// storage duration
int test_nested_auto_without_null_byte(void) {
    char nested[3][3] = {"yes", "no", "ok"};
    char *whole_array = (char *)nested;
    char *word1 = (char *)nested[0];
    char *word2 = (char *)nested[1];
    char *word3 = (char *)nested[2];
    // all strcmp calls should return 0
    return !(strcmp(whole_array, "yesno") || strcmp(word1, "yesno") ||
             strcmp(word2, "no") || strcmp(word3, "ok"));
}

int main(void) {
    if (!test_flat_static_with_null_byte()) {
        return 1;
    }

    if (!test_nested_static_with_null_byte()) {
        return 2;
    }

    if (!test_flat_auto_with_null_byte()) {
        return 3;
    }

    if (!test_nested_auto_with_null_byte()) {
        return 4;
    }

    if (!test_flat_static_without_null_byte()) {
        return 5;
    }

    if (!test_nested_static_without_null_byte()) {
        return 6;
    }

    if (!test_flat_auto_without_null_byte()) {
        return 7;
    }

    if (!test_nested_auto_without_null_byte()) {
        return 8;
    }
    return 0;
}