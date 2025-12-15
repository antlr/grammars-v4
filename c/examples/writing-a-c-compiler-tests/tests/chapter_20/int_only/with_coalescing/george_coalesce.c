/* Test that we coalesce pseudos into hard registers when they pass the George
 * test. In this case, coalescing lets us get rid of all moves between
 * registers. We inspect the assembly for the target function to validate that
 * it contains no spills and no mov instructions whose source and destination
 * are both general purpose registers (except mov %rsp, %rbp and mov %rbp, %rsp
 * in the prologue and epilogue).
 *
 * This test was generated from templates/chapter_20_templates/george_coalesce.c.jinja.
 * */

#include "../util.h"

int glob = 1;

int increment_glob(void) {
    glob = glob + 1;
    return 0;
}

/* 1. Validate six function parameters.
 *    Purpose: make sure they're coalesced into param-passing registers.
 * 2. Define and validate twelve other pseudoregisters that are preserved across
 *    function calls and have significant degree. Purpose: make sure the
 *    caller-saved hard registers (including EAX and param-passing registers)
 *    all have more than 12 neighbors with significant degree, so the coalescing
 *    decisions we validate in steps 1 & 3 depend on
 *    the George test and not Briggs. Otherwise, this test might succeed even
 *    if we weren't using the George test.
 * 3. Calculate five pseudoregisters and pass them into check_five_ints.
 *    Purpose: make sure they're coalesced into param-passing registers.
 * 4. Call check_one_int and return the result.
 *    Purpose: make sure return value is coalesced into EAX.
 */
int target(int a, int b, int c, int d, int e, int f) {
    // Validate parameters a-f (not with check_* functions, to avoid adding
    // new interference)
    if (a != 1) {
        return 1;
    }
    if (b != 2) {
        return 2;
    }
    if (c != 3) {
        return 3;
    }
    if (d != 4) {
        return 4;
    }
    if (e != 5) {
        return 5;
    }
    if (f != 6) {
        return 6;
    }

    // Create/validate a bunch of callee-saved pseudos with significant degree.

    // Initialize these using glob, instead of id(), to avoid mov instructions
    // between registers (e.g. movl %eax, %callee1)
    int one = glob * 1;
    int two = glob * 2;
    int three = glob * 3;
    int four = glob * 4;
    // validate these to increase their degree more
    if (one != 1) {
        return 7;
    }
    if (two != 2) {
        return 8;
    }
    if (three != 3) {
        return 9;
    }
    if (four != 4) {
        return 10;
    }
    // force them to be callee-saved
    increment_glob();
    // define new vars using the old ones, then validate them
    int five = 4 + one;
    int six = 4 + two;
    int seven = 4 + three;
    int eight = 4 + four;
    if (five != 5) {
        return 11;
    }
    if (six != 6) {
        return 12;
    }
    if (seven != 7) {
        return 13;
    }
    if (eight != 8) {
        return 14;
    }
    increment_glob();
    // define one last batch
    int nine = 14 - five;
    int ten = 16 - six;
    int eleven = 18 - seven;
    int twelve = 20 - eight;
    // force them to be callee-saved
    increment_glob();
    // validate them
    if (nine != 9) {
        return 15;
    }
    if (ten != 10) {
        return 16;
    }
    if (eleven != 11) {
        return 17;
    }
    if (twelve != 12) {
        return 18;
    }
    // Now make sure values passed as arguments are coalesced into
    // parameter-passing registers. Calculate using glob so we can't
    // copy prop or constant fold them, and don't need to mov values
    // between registers to calculate them.
    int s = glob - 3;  // 1
    int t = glob - 2;  // 2
    int u = glob - 1;  // 3
    int v = glob * 2 - 4;  // 4
    int w = glob + 1;  // 5
    check_5_ints(s, t, u, v, w, 1);


    // make sure return value is coalesced into EAX
    return check_one_int(glob, 4);
}