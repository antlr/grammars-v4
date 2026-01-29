/* Eliminate unreachable code witihn a switch statement body */

int callee(void) {
    return -1;
}

int target(int x) {
    int retval = 0;
    switch (x) {
    case 1:
        retval = 1; break;
    case 2: retval = 2; break;
        callee(); // unreachable - occurs after 'break' from previous case and before next one
    case 3: retval = 10; break;
    default: return -1;
        callee(); // unreachable
    }

    return retval;


}

int main(void) {
    return target(3);
}