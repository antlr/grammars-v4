double glob = 3.0;

int main(void) {

    /* this is a test for a bug I found in my reference implementation:
     * I rewrote
     *   cvttsd2sil -8(%rbp), -12(%rbp)
     * as
     *   cvttsd2sil -8(%rbp), %r11d
     *   movq       %r11, -12(%rbp)
     * which potentially clobbered other things on the stack (e.g. at -8(%rbp))
     * we include other variables on the stack in order to catch this
     */
    long l = -1l;
    int i = -1;
    int j = (int) glob;
    int k = 20;
    if (l != -1l) {
        return 1;
    }
    if (i != -1) {
        return 2;
    }
    if (j != 3) {
        return 3;
    }
    if (k != 20) {
        return 4;
    }
    return 0;
}