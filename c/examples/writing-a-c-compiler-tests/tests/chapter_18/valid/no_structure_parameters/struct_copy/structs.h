
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);
void *malloc(unsigned long size);

struct small {
    int a;
    long b;
};

struct s {
    char arr[3];
    struct small inner;
};

struct with_end_padding {
    int a;
    int b;
    char c;
};