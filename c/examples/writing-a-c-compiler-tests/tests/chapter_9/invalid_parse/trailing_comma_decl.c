/* Trailing commas aren't permitted in parameter lists */
int foo(int a,) {
    return a + 1;
}

int main(void) {
    return foo(4);
}