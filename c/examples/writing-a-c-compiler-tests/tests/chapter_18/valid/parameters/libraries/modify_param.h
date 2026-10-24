/* Modify a parameter of structure type
 * */

struct inner {
    double d;
    int i;
};

struct outer {
    struct inner s;
    struct inner *ptr;
    long l;
};

int modify_simple_struct(struct inner s);
int modify_nested_struct(struct outer s);