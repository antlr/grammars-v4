/* Test how we handle copies in the transfer function:
 * x = src should generate a copy _and_ kill previous copies
 * where x is the source or destination
 * */
static int globvar;

int set_globvar(int i) {
    globvar = i;
    return 0;
}

int callee(int a, int b) {
    return a + b;
}

int target(int param) {
    int x = param;
    // should be able to propagate param into var but we don't explicitly
    // check that here
    set_globvar(x);
    int y = x;  // gen y = x;
    x = 10;     // kill x = param and y = x, gen x = 10
    // make sure we propagate x = 10 but not y = x
    return callee(x, y);  // becomes callee(10, y)
}

int main(void) {
    if (target(4) != 14) {
        return 1;
    }
    if (globvar != 4) {  // make sure we passed the right value to set_globvar
        return 2;
    }
    return 0;  // success
}