int foo(void) {
    return 2;
}

int main(void) {
    int x = 3;
    x -= foo();
    return x;
}