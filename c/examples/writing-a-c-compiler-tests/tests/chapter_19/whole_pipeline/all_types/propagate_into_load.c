/* Make sure we can propagate copies into Load instruction.
 * in assembly for target, we'll see a copy to glob but no reads from it
 */

int *glob;
int i = 10;
int target(void) {
    int *loc = &i;
    glob = loc;
    return *glob;  // rewrite as *loc; don't need to read glob here
}

int main(void) {
    if (target() != 10) {
        return 1;  // failure
    }
    if (*glob != 10) {
        return 2;  // failure
    }

    return 0;  // success
}