/* If two identical copies to x appear on the path to some use of x,
 * and the first one is killed, make sure we can still propagate the second.
 * */

int x = 0;
int y = 0;

int callee(void) {
    y = x * 2;  // make sure x still has the right value at this point
    return 5;
}

int target(void) {
    x = 2;         // gen x = 2
    x = callee();  // kill x = 2
    x = 2;         // gen x = 2 again
    return x;      // should become "return 2"
}

int main(void) {
    int result = target();
    if (result != 2) {
        return 1;
    }
    if (y != 4) {  // make sure we called callee()
        return 2;
    }
    if (x != 2) {
        return 3;
    }
    return 0;  // success
}