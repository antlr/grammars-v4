/* Test constant folding >> with negative source value (make sure
 * we perform an arithmetic rather than logical bit shit)
 */

int target(void) {
    return -20000 >> 3;
}

int main(void) {
    if (target() != -2500) {
        return 1;
    }

    return 0; // success
}