// Validate that all static union initializers, including nested ones,
// are constant

union u {
    long l;
};

struct has_union {
    int a;
    union u b;
    char c;
};

long some_var = 10l;

struct has_union some_struct = {1,
                                {some_var},  // INVALID - not constant
                                'a'};