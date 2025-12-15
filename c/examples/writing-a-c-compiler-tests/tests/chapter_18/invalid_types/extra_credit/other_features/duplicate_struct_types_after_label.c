// A label doesn't start a new scope, so you can't declare a new structure type
// after it
int main(void) {
    struct s {
        int a;
    };
foo:;
    // illegal redeclaration; struct s already declared in this scope
    struct s {
        int b;
    };
    return 0;
}