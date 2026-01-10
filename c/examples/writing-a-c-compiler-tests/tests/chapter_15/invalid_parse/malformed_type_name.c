int main(void) {
    int a = 4;
    int *foo = &a;
    // invalid cast syntax; missing type specifier
    int *bar[3] = (*[3]) foo;
    return 0;
}