/* Make sure our analysis recognizes which registers are used by each function
 * call. The test script validates that we don't spill.
 * Liveness analysis should recognize that only EDI, ESI,
 * and EDX are live right before we call callee(). If we assume ECX, R8D and R9D
 * are also live, we'll conclude that they're live from the start of the
 * function until the function call (since they're never updated) and won't be
 * able to allocate them, resulting in spills.
 * */

#include "../util.h"

int glob1;
int glob2;
int glob3;
int glob4;
int glob5;
int glob6;
int glob7;
int glob8;
int glob9;

// defined in tests/chapter_20/helper_libs/track_arg_registers_lib.c,
// exits early if a, b, c don't have expected values
int callee(int a, int b, int c);

// Note: we deliberately give target the same number of params as callee;
// if liveness incorrectly thought that some reg was used by callee and
// therefore live, it still wouldn't interfere with the parameter passed to
// target in that reg, so the error wouldn't necessarily force a spill. (I think
// having _fewer_ params in target than in callee would be be fine.)
int target(int one, int two, int three) {
    /* Create a clique of 12 pseudos, and pass
     * three of them to callee.
     * */

    int four = two + 2;
    int five = three + two;
    int six = 12 - one - two - three;
    int seven = 13 - six;
    int eight = four * two;
    int nine = three * three;
    int ten = six + four;
    int eleven = six * two - one;
    int twelve = six * two;

    // to make sure they all interfere without forcing them to be callee-saved,
    // copy all of them to global variables
    glob1 = one;
    glob2 = two;
    glob3 = three;
    glob4 = four;
    glob5 = five;
    glob6 = six;
    glob7 = seven;
    glob8 = eight;
    glob9 = nine;
    // don't need to copy in ten through twelve b/c we use them below

    // use ten through twelve
    callee(ten, eleven, twelve);

    // validate globals
    check_12_ints(glob1, glob2, glob3, glob4, glob5, glob6, glob7, glob8, glob9,
                  ten, eleven, twelve, 1);

    return 0;
}