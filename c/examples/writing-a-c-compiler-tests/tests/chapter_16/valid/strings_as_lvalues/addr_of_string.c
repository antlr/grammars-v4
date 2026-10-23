/* Test that we can take the address of a string literal and annotate it with the correct type */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int puts(char *s);

int main(void) {
    char(*str)[16] = &"Sample\tstring!\n";
    puts(*str);

    // get pointer to one-past-the-end of this string
    char (*one_past_the_end)[16] = str + 1;
    char *last_byte_pointer = (char *)one_past_the_end - 1; // now get pointer to the last byte
    if (*last_byte_pointer != 0) {
        return 1;
    }
    return 0;
}