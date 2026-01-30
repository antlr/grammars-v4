/* Test that we classify structure parameters correctly,
 * by passing a variety of structures as arguments.
 * Each test function takes only one argument.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);

// from Listing 18-39
struct twelve_bytes {
    int i;
    char arr[8];
};  // two INTEGER eightbytes

// from Listing 18-40
struct inner {
    int i;
    char ch2;
};

struct nested_ints {
    char ch1;
    struct inner nested;
};  // two INTEGER eightbytes

// from Listing 18-41
struct flattened_ints {
    char c;
    int i;
    char a;
};  // two INTEGER eightbytes

// From uncaptioned listing in "Classifying Eightbytes" section
struct large {
    int i;
    double d;
    char arr[10];
};  // four MEMORY eightbytes

// Three structure declarations from Listing 18-42
struct two_ints {
    int i;
    int i2;
};  // one INTEGER eightbyte

struct nested_double {
    double array[1];
};  // one SSE eightbyte

struct two_eightbytes {
    double d;
    char c;
};  // one SSE eightbyte, one INTEGER eightbyte

// From Listing 18-47
struct pass_in_memory {
    double w;
    double x;
    int y;
    long z;
};  // four MEMORY eightbytes

// validation functions defined in library
int test_twelve_bytes(struct twelve_bytes s);
int test_nested_ints(struct nested_ints s);
int test_flattened_ints(struct flattened_ints s);
int test_large(struct large s);
int test_two_ints(struct two_ints s);
int test_nested_double(struct nested_double s);
int test_two_eightbytes(struct two_eightbytes s);
int test_pass_in_memory(struct pass_in_memory s);