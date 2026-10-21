/* Test that we concatenate adjacent string literal tokens */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int puts(char *s);

int main(void) {
    char *strings = "Hello," " World";
    puts(strings);
    return 0;
}