int main(void) {
    struct s {
        int a;
    };
    struct s x = {10};
    // postfix operatpors have higher precedence then prefix operators,
    // so this is equivalent to &(x->a),
    // which is invalid because x isn't a pointer.
    // This is really a test for the parser, not the type checker
    return &x->a;
}