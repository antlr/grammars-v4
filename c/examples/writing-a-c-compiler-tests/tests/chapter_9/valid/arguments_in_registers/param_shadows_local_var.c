int main(void) {
    int a = 10;
    // a function declaration is a separate scope,
    // so parameter 'a' doesn't conflict with variable 'a' above
    int f(int a);
    return f(a);
}

int f(int a) {
    return a * 2;
}