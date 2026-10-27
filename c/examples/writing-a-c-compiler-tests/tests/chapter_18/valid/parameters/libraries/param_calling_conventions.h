/* Test that we can pass a mix of struct and non-struct arguments according to
 * the ABI */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);
int strncmp(char *s1, char *s2, unsigned long n);

// This type comes from Listing 18-45
struct two_longs {
    long a;
    long b;
};

struct one_int {
    int i;
    char c;
};

struct one_int_exactly {
    unsigned long l;
};

struct two_ints {
    char c;
    int arr[3];
};

struct two_ints_nested {
    struct one_int a;
    struct one_int b;
};

struct twelve_bytes {
    int i;
    char arr[8];
};

struct one_xmm {
    double d;
};

struct two_xmm {
    double d[2];
};

struct int_and_xmm {
    char c;
    double d;
};

struct xmm_and_int {
    struct one_xmm dbl;
    char c[3];
};

struct odd_size {
    char arr[5];
};

struct memory {
    double d;
    char c[3];
    long l;
    int i;
};

// passing structures as parameters

// all arguments fit in registers
int pass_small_structs(struct two_xmm two_xmm_struct, struct one_int int_struct,
                       struct one_xmm xmm_struct,
                       struct xmm_and_int mixed_struct,
                       struct twelve_bytes int_struct_2,
                       struct one_int_exactly another_int_struct);

// based on example in Listing 18-45
int a_bunch_of_arguments(int i0, int i1, int i2, int i3, int i4,
                         struct two_longs param, int i5);

// use remaining structure types, mix with scalars
int structs_and_scalars(long l, double d, struct odd_size os, struct memory mem,
                        struct one_xmm xmm_struct);

// pass fourth_struct in memory b/c we're out of XMM registers
int struct_in_mem(double a, double b, double c, struct xmm_and_int first_struct,
                  double d, struct two_xmm second_struct, long l,
                  struct int_and_xmm third_struct,
                  struct one_xmm fourth_struct);

// pass two_ints_nested in memory - we have one general-purpose reg left for
// parameter passing but it requires two
int pass_borderline_struct_in_memory(struct two_ints t_i, int c,
                                     struct int_and_xmm i_x, void *ptr,
                                     struct two_ints_nested t_i_n, double d);

// pass a struct in memory that isn't neatly divisible by 8
int pass_uneven_struct_in_mem(struct twelve_bytes struct1, long a, long b,
                              struct twelve_bytes struct2, struct odd_size os,
                              struct memory m);

// pass first struct in memory, later structs in registers
int pass_later_structs_in_regs(struct memory m, struct twelve_bytes struct1, struct one_xmm struct2);