int foo(int a) {
    /* A function's parameter list and its body are in the same scope,
     * so redeclaring a here is illegal. */
    int a = 5;
    return a;
}

int main(void) {
    return foo(3);
}