/* Test that we can declare a function with an incomplete parameter type,
 * then call/define it after the type is completed */
struct s;

// struct s is incomplete here, so we can declare this function
// but not define it
int foo(struct s blah);

// complete the type
struct s {
    int a;
    int b;
};

int main(void) {
    struct s arg = {1, 2};
    return foo(arg); // we can call foo b/c 'struct s' type is completed
}

int foo(struct s blah) {
    return blah.a + blah.b;
}