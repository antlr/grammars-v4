#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

// library functions
int strcmp(char *s1, char *s2);

union simple {
    double d;
    char c;
    int *ptr;
};

union inner {
    char arr[9];
};

struct my_struct {
    long l;
    union inner u;
    int i;
};

union nested {
    struct my_struct str;
    union simple s;
    long l;
};

int validate_simple(union simple *ptr);
int validate_simple_converted(union simple *ptr);
int validate_nested(union nested *ptr);
int validate_nested_partial(union nested *ptr);
