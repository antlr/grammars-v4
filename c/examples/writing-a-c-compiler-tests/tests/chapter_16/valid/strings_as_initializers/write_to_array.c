// Test writing to a char array

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int puts(char *s);

int main(void) {
    // start with a flat array
    char flat_arr[4] = "abc";
    puts(flat_arr);

    // update it
    flat_arr[2] = 'x';
    puts(flat_arr);

    // similar test with nested array
    char nested_array[2][6] = {"Hello", "World"};
    puts(nested_array[0]);
    puts(nested_array[1]);

    nested_array[0][0] = 'J';
    puts(nested_array[0]);

    return 0;
}