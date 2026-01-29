void f(void);

int main(void) {
    // the condition in a ternary expression must have scalar type
    return f() ? 1 : 2;
}