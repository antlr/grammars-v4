static int f(void) {
    goto x;
    return 0;
    x:
    return 2;
}

int f_caller(void) {
    return f();
}