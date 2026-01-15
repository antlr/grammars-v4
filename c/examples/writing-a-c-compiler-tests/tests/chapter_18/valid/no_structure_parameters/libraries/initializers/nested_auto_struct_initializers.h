/* Test initialization of nested structs with automatic storage duration,
 * including:
 * - partial initialization
 * - using mix of compound and single initializers to initialize nested structs
 * - arrays of structs, structs containing arrays
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);

// struct type defs
struct pair {
    int a;
    int b;
};

struct inner {
    int one_i;
    unsigned char two_arr[3];
    unsigned three_u;
};

struct outer {
    long one_l;
    struct inner two_struct;
    char *three_msg;
    double four_d;
    struct pair five_pair;
};

// validation functions defined in library
int validate_full_initialization(struct outer *ptr);
int validate_partial_initialization(struct outer *ptr);
int validate_mixed_initialization(struct outer *ptr);
int validate_array_of_structs(struct outer *struct_array);