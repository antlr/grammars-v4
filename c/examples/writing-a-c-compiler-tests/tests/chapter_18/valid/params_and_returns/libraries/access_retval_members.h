/* Test for accessing the members in a return value of structure type */
struct inner {
    char x;
    long y;
};

struct outer {
    double d;
    struct inner *ptr;
    struct inner s;
};

void *calloc(unsigned long nmemb, unsigned long size);

struct inner return_small_struct(void);
struct outer return_nested_struct(void);