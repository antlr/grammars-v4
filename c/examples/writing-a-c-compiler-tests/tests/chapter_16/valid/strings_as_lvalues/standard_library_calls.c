/* Test calling string manipulation functions from the standard library */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);
int puts(char *s);
unsigned long strlen(char *s);
int atoi(char *s);

int main(void) {
    if (strcmp("abc", "abc")) {
        return 1;
    }

    // "ab" should compare less than "xy"
    if (strcmp("ab", "xy") >= 0) {
        return 2;
    }

    puts("Hello, World!");

    if (strlen("")) {
        return 3;
    }

    int i = atoi("10");
    if (i != 10) {
        return 4;
    }

    return 0;
}