// rval in compound expression cannot be void
void f(void) {
    return;
}

int main(void) {
    int *x = 0;
    x += f();
    return 0;
}