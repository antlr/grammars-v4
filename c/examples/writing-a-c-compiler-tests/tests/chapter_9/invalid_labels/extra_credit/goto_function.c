int foo(void) {
    return 3;
}

int main(void) {
    /* You can't use a function name as a goto label */
    goto foo;
    return 3;
}