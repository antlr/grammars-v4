// Make sure members in static structures retain their values
// across multiple function invocations

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

void *malloc(unsigned long size);
int putchar(int ch);
int puts(char *s);

// test that changes to static struct are retained across function calls
// do this by validating text written to stdout,
// instead of usual pattern of signifying success/failure with return value
void test_static_local(int a, int b) {
    struct s {
        int a;
        int b;
    };

    static struct s static_struct;
    if (!(static_struct.a || static_struct.b)) {
        puts("zero");
    } else {
        putchar(static_struct.a);
        putchar(static_struct.b);
        putchar('\n');
    }

    static_struct.a = a;
    static_struct.b = b;
}

// test that changes to struct made through static pointer are retained across
// function calls do this by validating text written to stdout
void test_static_local_pointer(int a, int b) {
    struct s {
        int a;
        int b;
    };

    static struct s *struct_ptr;
    if (!struct_ptr) {
        struct_ptr = malloc(sizeof(struct s));
    } else {
        putchar(struct_ptr->a);
        putchar(struct_ptr->b);
        putchar('\n');
    }

    struct_ptr->a = a;
    struct_ptr->b = b;
}

// test that changes to global struct are visible across function calls
struct global {
    char x;
    char y;
    char z;
};

struct global g;

void f1(void) {
    g.x = g.x + 1;
    g.y = g.y + 1;
    g.z = g.z + 1;
}

void f2(void) {
    putchar(g.x);
    putchar(g.y);
    putchar(g.z);
    putchar('\n');
}

void test_global_struct(void) {
    g.x = 'A';
    g.y = 'B';
    g.z = 'C';

    f1();
    f2();
    f1();
    f2();
}

// test that changes to global struct pointer are visible across function calls
struct global *g_ptr;

void f3(void) {
    g_ptr->x = g_ptr->x + 1;
    g_ptr->y = g_ptr->y + 1;
    g_ptr->z = g_ptr->z + 1;
}

void f4(void) {
    putchar(g_ptr->x);
    putchar(g_ptr->y);
    putchar(g_ptr->z);
    putchar('\n');
}

void test_global_struct_pointer(void) {
    g_ptr = &g;  // first, point to global struct from previous test
    f3();
    f4();
    f3();
    f4();
    // now declare a new struct and point to that instead
    g_ptr = malloc(sizeof(struct global));
    g_ptr->x = 'a';
    g_ptr->y = 'b';
    g_ptr->z = 'c';
    f3();
    f4();
    f3();
    f4();
}

int main(void) {
    test_static_local('m', 'n');
    test_static_local('o', 'p');
    test_static_local('!', '!');
    ;  // last one, won't be printed
    test_static_local_pointer('w', 'x');
    test_static_local_pointer('y', 'z');
    test_static_local_pointer('!', '!');
    ;  // last one, won't be printed
    test_global_struct();
    test_global_struct_pointer();
    return 0;
}