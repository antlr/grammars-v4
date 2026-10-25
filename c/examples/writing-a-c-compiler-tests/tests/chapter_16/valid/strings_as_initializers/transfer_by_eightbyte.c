#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif
/* Test that when we initialize an array whose size isn't divisible by 4 or 8,
 * we don't overrun neighboring memory
 */

int strcmp(char *s1, char *s2);

int main(void) {
    char strings[2][13] = {"abcdefghijkl", "z"};
    if (strcmp(strings[0], "abcdefghijkl"))
        return 1;

    if (strings[1][0] != 'z')
        return 2;

    // remaining bytes should be 0
    for (int i = 1; i < 13; i = i + 1) {
        if (strings[1][i])
            return 3;
    }
    return 0;
}