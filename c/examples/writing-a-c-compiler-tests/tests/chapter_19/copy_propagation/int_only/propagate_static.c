/* Test that we can propagate copies to variables with static storage
 * duration */
int x = 0;

int target(void) {
    // we can propagate value of x, even though it has static storage duration,
    // b/c no intervening reads/writes
    x = 10;
    return x;  // should become "return 10"
}

int main(void) {
    int result = target();
    if (result != 10) {
        return 1;
    }
    if (x != 10) {
        return 2;
    }
    return 0;  // success
}