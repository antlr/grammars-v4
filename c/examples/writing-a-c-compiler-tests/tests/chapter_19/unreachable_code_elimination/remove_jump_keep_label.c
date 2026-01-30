/* Test that we remove one jump to a label without necessarily removing that
 * label, since other blocks may also jump to it.
 * We don't inspect the assembly for this function,
 * so the function under test is 'f' insetad of 'target'
 * */

int x = 0;
int callee(void) {
    x = x + 1;
    return 0;
}

int f(void) {
    for (int i = 0; i < 10; i = i + 1) {
        if (0) {
            // we'll optimize away this break, which jumps to this loop's
            // break label; however, we shouldn't optimize away the break label
            // because we still jump to it when we exit the loop normally
            break;
        }
        callee();
    }
    return 0;
}

int main(void) {
    f();
    return x;
}