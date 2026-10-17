/* Test case where we can't propagate a value into a case statement,
 * because it's reachable from either start of switch or fallthrough from
 * previous case statement, where the values differ.
 * */

int target(int flag) {
    int retval = 10;
    switch(flag) {
        case 1:
        retval = 0;
        case 2:
        // can't propagate - retval could be eitehr 10 or 0
        return retval;
        default: return -1;
    }
}

int main(void) {
    return target(1);
}