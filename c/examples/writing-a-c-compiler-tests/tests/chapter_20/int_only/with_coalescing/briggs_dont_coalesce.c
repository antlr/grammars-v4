/* Test that we don't coalesce two pseudos that fail the Briggs test. In this
 * case, coalescing two candidates that are connected by a mov instruction would
 * cause a spill, so we inspect the assembly for the target function to make
 * sure there are no spills.
 * */

#include "../util.h"

int glob = 5;

int update_glob(void) {
    glob = glob + 1;
    return 0;
}

int target(void) {
    // NOTE: parts of this are copy pasted from loop.c, which creates a similar
    // interference graph
    int z = glob + 10;
    int a;

    // define four callee-saved regs
    int one = glob - 4;
    int two = glob / 2;
    int three = -glob + 8;
    int four = glob - 1;

    update_glob(); // make one-four and z callee-saved

    if (glob) {
        // a and z are both callee-saved but their live ranges don't overlap;
        // we can avoid spills by placing them in the same hard register
        // this becomes:
        // movl $1, %tmp
        // subl %z, %tmp
        // movl %tmp, %a
        // so a and z don't interfere, but they will if we coalesce tmp and a.
        // Briggs test should prevent them from being coalesced.
        // we put this in an if statement to prevent copy prop of 1-z
       a = 1 - z;
    } else {
        a = 5;
    }


    // validate callee-saved regs
    check_one_int(one, 1);
    check_one_int(two, 2);
    check_one_int(three, 3);
    check_one_int(four, 4);

    // validate a
    check_one_int(a, -14);

    // validate glob
    check_one_int(glob, 6);
    return 0;
}