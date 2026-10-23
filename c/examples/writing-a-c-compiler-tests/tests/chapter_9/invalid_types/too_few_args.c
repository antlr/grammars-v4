int foo(int a, int b) {
    return a + 1;
}

int main(void) {
    /* foo is called with too many arguments */
    return foo(1);
}