/* Test that we add every block to the worklist
 * at the start of the iterative algoirthm
 * */

int global;

int flag = 1;

int f(void) {
    global = 100;
    return 0;
}

int main(void) {
    // Initially, we annotate every block with 'global = 0',
    // which is the only copy in 'main'.
    global = 0;

    // The copy 'global = 0' reaches this if statement. If we only add a block
    // to the worklist when its predecessor's outgoing copies change,
    // instead of adding every block to the initial worklist, we won't
    // visit this block at all, and we won't see the call to f(),
    // which kills this copy
    if (flag) {
        f();  // kill copy to global
    }

    // If we didn't visit that if statement, we'll incorrectly
    // rewrite this as 'return 0'.
    return global;
}
