/* Using a value in a comparison doesn't update it.
 * The test script validates that we don't spill any pseudos;
 * Exactly five callee-saved regs conflict at any time,
 * and treating cmp v1, v2 as update to v2 would force a spill
 * */

int glob0 = 0;
int glob1 = 1;
int glob2 = 2;
int glob3 = 3;
int glob4 = 4;

// use this to force pseudoregs to be callee-saved
int increment_globals(void) {
    glob0 = glob0 + 1;
    glob1 = glob1 + 1;
    glob2 = glob2 + 1;
    glob3 = glob3 + 1;
    glob4 = glob4 + 1;
    return 0;
}

int validate(int zero, int one, int two, int three, int four);

int target(void) {
    /* define 5 variables that all interfere
     * and must be in callee-saved regs
     */
    int a = glob0;
    int b = glob1;
    int c = glob2;
    int d = glob3;
    int e = glob4;
    increment_globals();  // to force a-e to be callee saved
    int x = a;            // now x interferes with b through e, but not a

    /* if cmp updated its second operand, this comparison would make
     * a interfere with x. Then we'd need to spill, because
     * all six callee-saved pseudos would interfere with each other
     * and only five callee-saved regs are available
     */
    if (a > b) {
        return 1;  // fail
    }
    if (b) {
        x = 100;  // so we can't propagate x = a into function call
    }

    increment_globals(); // to force x to be callee-saved
    return validate(x, b, c, d, e);
}

int validate(int hundred, int one, int two, int three, int four) {
    // validate global and local variables

    // first validate globals - each has been incremented twice
    if (glob0 != 2) {
        return 2;
    }

    if (glob1 != 3) {
        return 3;
    }

    if (glob2 != 4) {
        return 4;
    }

    if (glob3 != 5) {
        return 5;
    }

    if (glob4 != 6) {
        return 6;
    }

    // now validate locals
    if (hundred != 100) {
        return 7;
    }

    if (one != 1) {
        return 8;
    }

    if (two != 2) {
        return 9;
    }
    if (three != 3) {
        return 10;
    }
    if (four != 4) {
        return 11;
    }

    return 0;  // success
}