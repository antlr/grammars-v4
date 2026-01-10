/* Test that we performing coalescing between quadword pseudos that pass the
 * Briggs test. In this case, coalescing lets us get rid of all moves between
 * registers. We inspect the assembly for the target function to validate that
 * it contains no spills and no mov instructions whose source and destination
 * are both general-purpose registers (except mov %rsp, %rbp and mov %rbp, %rsp
 * in the prologue and epilogue).
 *
 * This test was generated from templates/chapter_20_templates/briggs_coalesce.c.jinja.
 * */

#include "../util.h"

long glob = 5l;

long glob7;
long glob8;
long glob9;
long glob10;
long glob11;
long glob12;

int target(long one, long two, long three, long four, long five, long six) {

    // Define 6 variables that interfere with each other and with arguments,
    // initializing each one with a complex expression that requires an
    // intermediate result. The pseudoregister holding each result should be
    // coalesced into the corresponding variable. Once these have been coalesced
    // only 12 pseudos will be left, and we'll have reduced the number of nodes
    // with significant degree by enough that we can coalesce all the arguments
    // into parameter-passing registers. This test coalesces temporary values
    // into 6 different variables, which must all be placed in different
    // registers, to validate that we actually performed coalescing and didn't
    // just happen to assign a variable and the corresponding intermediate
    // result to the same hard register.
    long seven = (glob - 2l) + four;
    long eight = (glob - 1l) * two;
    long nine = (glob - 2l) * three;
    long ten = (10l - glob) * two;
    long eleven = (glob * two) + one;
    long twelve = (glob + 1l) * two;

    // Save to global variables to validate later
    glob7 = seven;
    glob8 = eight;
    glob9 = nine;
    glob10 = ten;
    glob11 = eleven;
    glob12 = twelve;

    // Validate arguments
    check_12_longs(one, two, three, four, five, six, 7l, 8l, 9l, 10l, 11l, 12l,
                   1l);

    // Validate globals
    check_one_long(glob7, 7l);
    check_one_long(glob8, 8l);
    check_one_long(glob9, 9l);
    check_one_long(glob10, 10l);
    check_one_long(glob11, 11l);
    check_one_long(glob12, 12l);
    return 0;
}