int foo(int a);

int main(void) {
    return 5;
}

/* The forward declaration and definition of 'foo' conflict
 * (different numbers of parameters)
 */
int foo(int a, int b) {
    return 4;
}