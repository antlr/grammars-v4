/* Test that we can propagate copies where the source value is
 * a variable, in a function with no control flow strucures.
 * */
int callee(int a, int b) {
    return a + b;
}
int f(void) {
    return 3;
}

int globl = 0;
int set_globvar(void) {
    globl = 4;
    return 0;
}

int target(void) {
    int x = f();
    int y = x;  // propagate this copy into function call

    // call another function before callee so we can't coalesce x into EDI
    // or y into ESI; otherwise it will look like we've propagated x as
    // a function argument even if we haven't
    set_globvar();

    // look for: same value passed in ESI, EDI
    int sum = callee(x, y);

    // now update y while x is live, so we can't coalesce them
    // into the same register; otherwise it will look like we've propagated x as
    // a function argument even if we haven't
    y = f();
    return (sum + x * y);  // return 6 + 9 ==> 15
}

int main(void) {
    // make sure target gives correct result
    if (target() != 15) {
        return 1;
    }

    // make sure we called set_globvar
    if (globl != 4) {
        return 2;
    }
    return 0;  // success
}
