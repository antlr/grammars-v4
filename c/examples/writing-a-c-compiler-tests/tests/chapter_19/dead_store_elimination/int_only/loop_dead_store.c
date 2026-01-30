/* Test that we can detect dead stores in a function with a loop */
int putchar(int c);  // from standard library

int target(void) {
    int x = 5;   // dead store
    int y = 65;  // not a dead store
    do {
        x = y + 2;  // kill x, gen y
        if (y > 70) {
            // make sure we assign to x on multiple paths
            // so copy prop doesn't replace it entirely
            x = y + 3;
        }
        y = putchar(x) + 3;  // gen x and y
    } while (y < 90);
    if (x != 90) {
        return 1;  // fail
    }
    if (y != 93) {
        return 2;  // fail
    }
    return 0;  // success
}

int main(void) {
    return target();
}