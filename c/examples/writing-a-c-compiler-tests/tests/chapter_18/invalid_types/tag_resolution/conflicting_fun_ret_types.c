struct s;

// declare a function that returns the incomplete structure type 'struct s'
struct s foo(void);

int main(void) {
    struct s;  // declare a distinct incomplete struct type

    // illegal declaration: this conflicts w/ earlier declaration of foo
    // becaues it has a different return type (inner instead of outer 'struct
    // s')
    struct s foo(void);
    return 0;
}