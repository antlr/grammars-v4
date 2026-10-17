/* Test that we don't apply the George test to pseudoregisters.
 * This program includes one pseudoregister that must spill, which is connected
 * by mov instructions to two other pseudoregisters. If we used the George test
 * to decide when to coalesce pseudos, we'd coalesce the pseudo that will spill
 * with one or both of these other pseudos, which would increase the number of
 * spill instructions in the program. We'll inspect the assembly for target to
 * make sure it has no more than the expected number of spill instructions.
 * */

#include "../util.h"  // declares check_* and id functions

// TODO consider refactoring with spill_test_metric, some of this is copy-pasted
// from there

int target(void) {
    // Define a clique of 6 callee-saved values, a-f. We should spill
    // c because it has the lowest spill cost. Another variable, x,
    // is connected to c by a mov instruction, and only interferes with
    // registers that also interfere with c. If we apply the George test to x
    // c (which would be an error because they're both pseudos) then c will
    // still spill, but the spill cost will be higher.
    int a = id(1);
    int b = id(2);

    // define and validate x
    int x = id(10);
    check_one_int(x, 10);

    // copy x to c
    int c = x;
    if (!a) {
        // this branch won't be taken; it's here to prevent copy prop of x
        // to later uses of c
        c = 100;
    }
    int d = id(3);
    int e = id(4);
    int f = id(5);

    // now use a, b, d, e, and f a bunch of times, so they'll have a higher
    // spill cost than c even if we coalesce another pseudo into x
    check_5_ints(a, b, d, e, f, 1);
    check_5_ints(a + 3, b + 3, d + 3, e + 3, f + 3, 4);
    check_5_ints(a + 4, b + 4, d + 4, e + 4, f + 4, 5);
    check_one_int(a * 2, 2);
    check_one_int(b * 2, 4);
    check_one_int(d * 2, 6);
    check_one_int(e * 2, 8);
    check_one_int(f * 2, 10);
    check_one_int(a * 3, 3);
    check_one_int(b * 3, 6);
    check_one_int(d * 3, 9);
    check_one_int(e * 3, 12);
    check_one_int(f * 3, 15);

    // next, define y, another variable that is connected to c by a mov but
    // shouldn't be coalesced into it.
    int g = c;
    if (!f) {
        // this branch won't be taken; it's here to prevent copy prop of c
        // to later uses of c
        g = -1;
    }

    // validate g
    check_one_int(g, 10);

    return 0;
}