/* Test that we return a wide range of struct types according to the ABI */
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

int strcmp(char *s1, char *s2);
int strncmp(char *s1, char *s2, unsigned long n);

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

// returning structures

struct one_int return_int_struct(void);
struct twelve_bytes return_two_int_struct(void);
struct one_xmm return_double_struct(void);
struct two_xmm return_two_double_struct(void);
struct xmm_and_int return_mixed(void);
struct int_and_xmm return_mixed2(void);
struct memory return_on_stack(void);

// return on stack + pass other int params
struct memory pass_and_return_regs(int i, double d, struct int_and_xmm strct,
                                   int c, struct two_ints t_i, long l,
                                   struct one_int_exactly o_i_e, int c2);