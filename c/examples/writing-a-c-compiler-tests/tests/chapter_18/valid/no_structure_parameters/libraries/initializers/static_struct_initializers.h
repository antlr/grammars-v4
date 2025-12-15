/* Test initialization of non-nested static structs, including:
 * - partial initialization
 * - implicit conversion of scalar elements
 * - array decay of string literals
 */
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);

struct s {
    double one_d;
    char *two_msg;
    unsigned char three_arr[3];
    int four_i;
};

// static structures defined in client
extern struct s uninitialized;
extern struct s partial;
extern struct s partial_with_array;
extern struct s converted;

// validation functions defined in library

// case 1: struct with no explicit initializer should be all zeros
int test_uninitialized(void);

// case 2: partially initialized struct
int test_partially_initialized(void);

// case 3: partially initialized array w/in struct
int test_partial_inner_init(void);

// case 4: implicit conversion of scalar elements
int test_implicit_conversion(void);