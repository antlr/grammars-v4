/* Test that having empty blocks after optimization doesn't break anything;
 * after removing useless jumps and labels, 'target' will contain several
 * empty basic blocks.
 * */

int target(int x, int y) {
    if (x) {
        if (y) {
        }
    }
    return 1;
}

int main(void) {
    if (target(1, 1) != 1) {
        return 1; // fail
    }
    if (target(0,0) != 1) {
        return 2; // fail
    }
    return 0; // success
}