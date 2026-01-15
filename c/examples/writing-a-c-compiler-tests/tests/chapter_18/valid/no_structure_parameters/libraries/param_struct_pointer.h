/* Test passing struct pointers as function parameters */

// library function
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

// library function declarations
int access_members_through_pointer(struct outer *ptr, int expected_a,
                                   int expected_b, double expected_d,
                                   int expected_i);

void update_members_through_pointer(struct outer *ptr, int a, int b,
                                    struct inner *inner_ptr);