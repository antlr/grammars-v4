/* Test initialization of nested static structs, including:
 * - partial initialization
 * - arrays of structs, structs containing arrays
 * - implicit conversion of scalar elements, array decay of string literals
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

// standard library function
int strcmp(char *s1, char *s2);

// structure type defs
struct inner {
    int one_i;
    signed char two_arr[3];
    unsigned three_u;
};

struct outer {
    long one_l;
    struct inner two_struct;
    char *three_msg;
    double four_d;
};

// declarations of global vars (defined in client)
extern struct outer all_zeros;
extern struct outer partial;
extern struct outer full;
extern struct outer converted;
extern struct outer struct_array[3];

// declarations of validation functions (defined in lib)
int test_uninitialized(void);
int test_partially_initialized(void);
int test_fully_intialized(void);
int test_implicit_conversions(void);
int test_array_of_structs(void);
