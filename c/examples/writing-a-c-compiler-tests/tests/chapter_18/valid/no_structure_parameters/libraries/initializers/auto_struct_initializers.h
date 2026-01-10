/* Test initialization of non-nested structs with automatic storage duration,
 * including:
 * - partial initialization
 * - implicit type conversions
 * - compound and single expressions as initializers
 * - string literals as pointer and array initializers
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

// library functions
int strcmp(char *s1, char *s2);

void *malloc(unsigned long size);
void *calloc(unsigned long nmemb, unsigned long size);

// struct type def
struct s {
    char *one_msg;
    unsigned char two_arr[3];
    struct s *three_self_ptr;
    double four_d;
    double *five_d_ptr;
};

// validation functions defined in library
int validate_full_initialization(struct s *ptr);
int validate_partial_initialization(struct s *ptr, char *expected_msg);
int validate_converted(struct s *ptr);
int validate_two_structs(struct s *ptr1, struct s *ptr2);