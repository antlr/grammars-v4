struct s;

// declare a function that takes param with incomplete struct type
int foo(struct s x);

int main(void) {
    struct s;  // declare a different incomplete struct type

    // illegal declaration: this conflicts with earlier declaration of 'foo'
    // becasue it has a different type (second 'struct s' instead of first)
    int foo(struct s x);
    return 0;
}