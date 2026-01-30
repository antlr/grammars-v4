/* Anything that appears in a switch statement before the body of the first
 * case is unreachable.
 */

int callee(void) {
    return 0;
}

int target(int x) {
    switch(x) {
        return callee(); // unreachable
        case 1: return 1;
        default: return 2;
    }

}

int main(void) {
    return target(1);
}