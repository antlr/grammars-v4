int foo(void) {
    struct s {
        int a;
        int b;
    };
    struct s result = {1, 2};
    return result.a + result.b;
}

int main(void) {
    // previously defined struct s is not in scope here,
    // so this is declares a new incomplete type
    struct s;
    // this is illegal because it defines a variable with an incomplete type
    struct s blah = {foo(), foo()};
    return blah.a;
}