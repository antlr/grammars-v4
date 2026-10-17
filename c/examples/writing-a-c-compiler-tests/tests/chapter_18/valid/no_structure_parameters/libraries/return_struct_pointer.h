/* Test returning struct pointers from functions
 * and using struct pointers returned from functions
 * */

void *malloc(unsigned long size);

// define some struct types
struct inner {
    double d;
    int i;
};

struct outer {
    char a;
    char b;
    struct inner substruct;
};

struct outermost {
    int i;
    struct outer *nested_ptr;
    struct outer nested_struct;
};

// declare some functions that return pointers to structs
struct inner *make_struct_inner(int seed);
struct outer *make_struct_outer(int seed);
struct outermost *make_struct_outermost(int seed);