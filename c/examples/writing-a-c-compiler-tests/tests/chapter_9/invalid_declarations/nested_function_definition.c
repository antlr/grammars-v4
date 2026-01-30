int main(void) {
    /* Nested function definitions are not permitted */
    int foo(void) {
        return 1;
    }
    return foo();
}