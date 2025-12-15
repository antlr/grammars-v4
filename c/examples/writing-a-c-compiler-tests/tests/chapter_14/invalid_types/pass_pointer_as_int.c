/* It's illegal to pass a pointer as a function argument
 * where an integer is expected, beause you can't implicitly
 * convert a pointer to an integer type
 */
int f(int i) {
    return i;
}

int main(void) {
    int x;
    return f(&x);
}