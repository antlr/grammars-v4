/* Make sure we can propagate copies into CopyFromOffset instruction.
 * In assembly for target, we'll see a copy to glob but no reads from it
 */

struct s {
    int a;
    int b;
};

struct s glob;

int target(void) {
    struct s loc = {100, 200};

    glob = loc;

    int x = glob.b;  // rewrite as x = loc.b

    return x;
}

int main(void) {
    if (target() != 200) {
        return 1;  // failure
    }
    if (glob.a != 100) {
        return 2;  // failure
    }
    if (glob.b != 200) {
        return 3;  // failure
    }
    return 0;  // success
}