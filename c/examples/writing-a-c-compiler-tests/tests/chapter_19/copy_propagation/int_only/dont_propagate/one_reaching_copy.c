/* If a copy appears on one path to a block but not on all
 * paths to that block, it doesn't reach that block.
 * */

int three(void) {
    return 3;
}

int target(int flag) {
    int x;
    if (flag)
        x = 10;
    else
        x = three();
    // one predecessor contains copy x = 10, other predecessor contains no
    // copies to x, so no copies reach 'return x'
    return x;
}

int main(void) {
    if (target(1) != 10) {
        return 1;
    }
    if (target(0) != 3) {
        return 2;
    }
    return 0;  // success
}