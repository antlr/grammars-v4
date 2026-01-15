/* Test that we can cast expressions to void */

int x;

int set_x(int i) {
    x = i;
    return 0;
}

void do_nothing(void) {
    ;
}

int main(void) {
    (void) x; // cast a variable to void; this expression has no effect

    // cast to void discards this expression's value but we still need its side effect.
    (void) set_x(12);

    // you can cast an expression to void that's already void
    (void) do_nothing();
    return x;
}