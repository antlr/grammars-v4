/* Test that we performing coalescing between pseudos that pass the Briggs test.
 * In this case, coalescing lets us get rid of all moves between registers. We
 * inspect the assembly for the target function to validate that it contains no
 * spills and no mov instructions whose source and destination are both general-
 * purpose registers (except mov %rsp, %rbp and mov %rbp, %rsp in the prologue
 * and epilogue).
 *
 * This test was generated from templates/chapter_20_templates/briggs_coalesce.c.jinja.
 * */

#include "../util.h"

int glob = 5;

int glob7;
int glob8;
int glob9;
int glob10;
int glob11;
int glob12;

int target(int one, int two, int three, int four, int five, int six) {

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
    int seven = (glob - 2) + four;
    int eight = (glob - 1) * two;
    int nine = (glob - 2) * three;
    int ten = (10 - glob) * two;
    int eleven = (glob * two) + one;
    int twelve = (glob + 1) * two;

    // Save to global variables to validate later
    glob7 = seven;
    glob8 = eight;
    glob9 = nine;
    glob10 = ten;
    glob11 = eleven;
    glob12 = twelve;

    // Validate arguments
    check_12_ints(one, two, three, four, five, six, 7, 8, 9, 10, 11, 12, 1);

    // Validate globals
    check_one_int(glob7, 7);
    check_one_int(glob8, 8);
    check_one_int(glob9, 9);
    check_one_int(glob10, 10);
    check_one_int(glob11, 11);
    check_one_int(glob12, 12);
    return 0;
}