struct s;

// declare a function that takes param with incomplete struct type
int foo(struct s x);

int main(void) {
    union s;  // declare an incomplete union type w/ same tag

    // illegal declaration: this conflicts with earlier declaration of 'foo'
    // becasue it has a different type ( 'union s' instead of 'struct s')
    int foo(union s x);
    return 0;
}