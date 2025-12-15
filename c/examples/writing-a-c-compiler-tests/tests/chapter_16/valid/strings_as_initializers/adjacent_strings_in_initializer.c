/* Make sure the parser concatenates adjacent string literals */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);  // from standard library

int main(void) {
    char multi_string[6] =
        "yes"
        "no";  // can concatenate two string literals in an initializer
    char nested_multi_string[2][3] = {
        "a"
        "b",
        "c"
        "d"};  // first element is "ab", second is "cd"

    // validate multi_string
    if (strcmp(multi_string, "yesno"))
        return 1;
    if (strcmp(nested_multi_string[0], "ab"))
        return 2;
    if (strcmp(nested_multi_string[1], "cd"))
        return 3;
    return 0;
}