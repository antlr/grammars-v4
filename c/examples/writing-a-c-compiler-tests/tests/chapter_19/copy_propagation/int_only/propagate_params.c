/* Test that we can propagate copies both to and from function parameters;
 * similar to propagate_var, but with paramters instead of variables.
 * */
int callee(int a, int b) {
    return a * b;
}
int f(void) {
    return 3;
}
int globl = 0;
int set_globvar(void) {
    globl = 4;
    return 0;
}
int target(int a, int b) {
    b = a;  // propagate copy from a to b

    // call another function before callee so we can't coalesce a into EDI
    // or b into ESI; otherwise, once we implement register coalescing,
    // it will look like we've propagated the copy even if we haven't
    set_globvar();
    // look for: same value passed in ESI, EDI
    int product = callee(a, b);

    // now update b while a is live, so we can't coalesce them
    // into the same register; otherwise it will look like we've propagated
    // the copy even if we haven't
    b = f();
    return (product + a - b);  // return 5 * 5 + 5 - 3 ==> 27
}

int main(void) {
    if (target(5, 6) != 27) {
        return 1;
    }

    // make sure we called set_globvar
    if (globl != 4) {
        return 2;
    }

    return 0;  // success
}