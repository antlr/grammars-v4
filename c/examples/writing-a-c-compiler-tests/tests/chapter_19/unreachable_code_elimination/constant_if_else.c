/* Test that we eliminate an unreachable 'if' statement body.
 * This also tests that we won't eliminate a block if some, but not all,
 * of its precedessors are unreachable. The final 'return' statement's
 * predecessors include the 'if' branch (which is dead) and the 'else'
 * statement (which isn't).
 * */
int callee(void) {
    return 0;
}

int target(void) {
    int x;
    if (0)
        x = callee();
    else
        x = 40;
    return x + 5;
}

int main(void) {
    return target();
}