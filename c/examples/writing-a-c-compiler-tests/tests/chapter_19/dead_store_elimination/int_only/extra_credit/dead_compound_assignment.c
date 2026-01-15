/* Test that we delete compound assignment to dead variables */

int glob = 0;

int target(void) {
    int x = glob;
    x *= 20; // dead
    x = 10;
    return x;
}

int main(void) {
    return target();
}