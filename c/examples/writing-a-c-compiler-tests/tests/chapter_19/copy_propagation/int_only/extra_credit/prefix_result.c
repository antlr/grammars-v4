/* Make sure copy propagation can track that the result of ++x and the updated
 * value of x have the same value
 */

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
    int x = f(); // x = 3
    // now x and y should have same (tmp) value
    int y = ++x; // x and y are 4

    // call another function before callee so we can't coalesce x into EDI
    // or y into ESI; otherwise it will look like we've propagated x as
    // a function argument even if we haven't
    set_globvar();

    // look for: same value passed in ESI, EDI
    int sum = callee(x, y); // sum = 8


    // now update y while x is live, so we can't coalesce them
    // into the same register; otherwise it will look like we've propagated x as
    // a function argument even if we haven't
    y = f(); // y  = 3
    return (sum + x * y);  // return 8 + 12 ==> 20
}


int main(void) {
    // make sure target gives correct result
    if (target() != 20) {
        return 1;
    }

    // make sure we called set_globvar
    if (globl != 4) {
        return 2;
    }
    return 0;  // success
}