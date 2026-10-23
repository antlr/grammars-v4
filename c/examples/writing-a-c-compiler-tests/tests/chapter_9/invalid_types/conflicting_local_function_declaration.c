int bar(void);

int main(void) {
    /* Two local declarations of foo in 'main' and 'bar' conflict -
     * different numbers of parameters
     */
    int foo(int a);
    return bar() + foo(1);
}

int bar(void) {
    int foo(int a, int b);
    return foo(1, 2);
}