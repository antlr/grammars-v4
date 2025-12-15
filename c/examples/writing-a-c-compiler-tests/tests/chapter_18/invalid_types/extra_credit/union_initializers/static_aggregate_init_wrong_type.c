// Make sure we validate the types of nested aggregate inits within union
// initializers

struct one_elem {
    long l;
};
struct three_elems {
    int one;
    int two;
    int three;
};

union one_or_three_elems {
    struct one_elem a;
    struct three_elems b;
};

int main(void) {
    // invalid: first element of union is struct one_elem, which we can't
    // initialize with three-element initializer
    static union one_or_three_elems my_union = {{1, 2, 3}};
    return 0;
}