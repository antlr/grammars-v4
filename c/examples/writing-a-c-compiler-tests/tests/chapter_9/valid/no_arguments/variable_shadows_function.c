int main(void) {
    int foo(void);

    int x = foo();
    if (x > 0) {
        int foo  = 3;
        x = x + foo;
    }
    return x;
}

int foo(void) {
    return 4;
}