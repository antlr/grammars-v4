/* Test declaring and operating on an array of pointers to strings */
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);

int main(void) {
    char *strings[4] = {"yes", "no", "maybe"};
    if (strcmp(strings[0], "yes")) {
        return 1;
    }
    if (strcmp(strings[1], "no")) {
        return 2;
    }
    if (strcmp(strings[2], "maybe")) {
        return 3;
    }
    // last element not initialized, so it's a null pointer
    if (strings[3]) {
        return 4;
    }

    return 0;
}