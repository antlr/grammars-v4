// In our implementation, this function definition fails tag resolution because
// the 'struct s' type hasn't been declared. In a fully conforming
// implementation, it would fail because you can't declare incomplete parameter
// types in function definitions.
int foo(struct s x) {
    return 0;
}