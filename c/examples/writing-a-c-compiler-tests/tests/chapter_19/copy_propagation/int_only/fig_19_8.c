/* Test case based on Figure 19-8:
 * Make sure we run iterative algorithm until the results converge.
 * */
static int called_counter = 0;

int callee(int i) {
    if (i == 3 && called_counter == 0) {
        // we're on first loop iteration; iterate one more time
        called_counter = 1;
        return 1;
    }
    if (i == 4 && called_counter == 1) {
        // we're on second loop iteration; stop
        called_counter = 2;
        return 0;
    }

    // if we hit this point, something has gone wrong!
    // set called_counter to indicate error, then terminate loop
    called_counter = -1;
    return 0;
}

int target(void) {
    int y = 3;
    int keep_looping;
    do {
        // After analyzing each basic block once,
        // it will look like we could rewrite this as
        // x = callee(3), but once the algorithm converges
        // we'll know that isn't safe.
        keep_looping = callee(y);
        y = 4;
    } while (keep_looping);  // loop should terminate after first iteration
    return y;                // should become return 4
}

int main(void) {
    int result = target();
    if (result != 4) {
        return 1;
    }
    if (called_counter != 2) {
        return 2;
    }
    return 0;  // success
}