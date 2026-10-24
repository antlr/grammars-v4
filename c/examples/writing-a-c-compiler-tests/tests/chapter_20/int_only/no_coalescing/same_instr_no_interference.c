/* Test that addl x, y and similar do NOT make x and y interfere if x is dead
 * afterward. The test script validates that there are no spills; if we think
 * addl x, y always makes x and y interfere, we'll be forced to spill some
 * callee-saved pseudos, but if the interference graph is accurate, we'll
 * allocate every register without spilling.
 */

#include "../util.h" // declares check_* and id functions

int target(void) {
    /* define some values - must be in callee-saved regs */
    int a = id(2);
    int b = id(3);
    int c = id(4);
    int d = id(5);
    int e = id(6);

    // validate them/call function to force them into callee-saved regs
    check_5_ints(a, b, c, d, e, 2);

    int f = a * a;  // now f interferes w/ b, c, d, e but not a
    int g = b + b;  // now g interferes w/ d, c, e, f but not a or b
    int h = c - c;  // h interferes with d, e, f, g but not a, b, or c
    int i = d * d;  // i interferes with e, f, g, h but not a, b, c, d
    int j = e + e;  // j interferes with f, g, h, i, but not a, b, c, d

    // another function call to make sure f-j are callee-saved
    check_one_int(0, 0);

    check_one_int(f, 4);
    check_one_int(g, 6);
    check_one_int(h, 0);
    check_one_int(i, 25);
    check_one_int(j, 12);

    return 0;
}