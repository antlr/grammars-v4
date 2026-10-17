/* Test standard pointer operations on string literals
 * including pointer arithmetic and subscripting.
 */

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wstring-plus-int"
#endif


int main(void) {
    // subscript a string literal
    if ("abcdefg"[2] != 'c') {
        return 1;
    }

    // pointer arithmetic on a string literal
    char *ptr = "This is a string!" + 10;  // point to "string."
    if (*ptr != 's') {
        return 2;
    }

    if (ptr[6] != '!') {
        return 3;
    }

    if (ptr[7]) {  // null byte
        return 4;
    }

    // use a string literal in a controlling expression;
    // pointer to this literal is non-null
    if (!"Not a null pointer!") {
        return 5;
    }
}