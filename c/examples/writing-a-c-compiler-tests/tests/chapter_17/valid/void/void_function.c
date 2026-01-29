/* Test our support for functions with void return values */
int foo = 0;

void set_foo_to_positive_num(int a) {
    if (a < 0) {
        // even if we don't return a value,
        /// make sure return statement exits the function!
        return;
    }
    foo = a;
    return;
}
void do_nothing(void) {
    // no return statement
}

int main(void) {
    set_foo_to_positive_num(-2);
    if (foo) { // value of foo should still be 0
        return 1;
    }
    set_foo_to_positive_num(12);

    if (foo != 12) {
        return 2;
    }
    do_nothing();
    return 0;
}