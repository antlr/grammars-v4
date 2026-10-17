/* Test that when we need to spill a node, and all nodes have the same degree,
 * we spill the one with the lowest spill cost (i.e. fewest uses)
 */

#include "../util.h" // declares check_* and id functions

int target(void) {
    // Define 6 callee-saved regs that interfere with each other;
    // c is used least, so it should spill.
    // NOTE: we make c the spill candidate, rather than a or f,
    // to reduce the risk of a false negative if the compiler happens to
    // spill the first or last possible pseudo.
    int a = id(1);
    int b = id(2);
    int c = id(10);
    int d = id(3);
    int e = id(4);
    int f = id(5);

    // use c once
    check_one_int(c, 10);

    // use others a few times
    check_5_ints(a, b, d, e, f, 1);
    check_5_ints(a + 3, b + 3, d + 3, e + 3, f + 3, 4);
    check_one_int(a * 2, 2);
    check_one_int(b * 2, 4);
    check_one_int(d * 2, 6);
    check_one_int(e * 2, 8);
    check_one_int(f * 2, 10);
    return 0;
}
