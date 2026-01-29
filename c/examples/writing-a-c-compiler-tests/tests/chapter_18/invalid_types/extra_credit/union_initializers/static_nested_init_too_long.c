// Union initializers, including nested ones, must have exactly one element
union u {
    int a;
    long b;
};
struct s {
    int tag;
    union u contents;
};

struct s my_struct = {
    10,
    {1, 2}  // invalid - nested union initializer has two elements
};