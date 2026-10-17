/* Propagate a value that's defined in a default statement that we always
 * reach
 */

int globvar = 0;

int target(int x) {
    int retval = 0;
    switch (x) {
        case 1: globvar = 1;
        case 2: globvar = globvar + 3;
        case 3: globvar = globvar * 2;
        default: retval = 3; // we always reach this no matter which case we take
    }

    return retval; // replace with "return 3"
}

int main(void) {
    int retval = target(2);
    if (retval != 3) {
        return 1; // fail
    }
    if (globvar != 6) {
        return 2; // fail
    }

    return 0; // success
}