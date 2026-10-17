void foo(void) {
    return;
}

int main(void) {
    int a = 3;
    int flag = 4;
    // you can't have a ternary expression with only one void branch
    // although Clang/GCC both allow it
    flag ? foo() : (a = 3);
    return 0;
}