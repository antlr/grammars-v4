/* Test that we performing coalescing between floating-point pseudos that pass
 * the Briggs test. In this case, coalescing lets us get rid of all moves
 * between registers. We inspect the assembly for the target function to
 * validate that it contains no spills and no mov instructions whose source and
 * destination are both XMM registers.
 *
 * This test was generated from templates/chapter_20_templates/briggs_coalesce.c.jinja.
 * */

#include "../util.h"

double glob = 5.0;

double glob9;
double glob10;
double glob11;
double glob12;
double glob13;
double glob14;

int target(double one, double two, double three, double four, double five,
           double six, double seven, double eight) {

    // Define 6 variables that interfere with each other and with arguments,
    // initializing each one with a complex expression that requires an
    // intermediate result. The pseudoregister holding each result should be
    // coalesced into the corresponding variable. Once these have been coalesced
    // only 14 pseudos will be left, and we'll have reduced the number of nodes
    // with significant degree by enough that we can coalesce all the arguments
    // into parameter-passing registers. This test coalesces temporary values
    // into 6 different variables, which must all be placed in different
    // registers, to validate that we actually performed coalescing and didn't
    // just happen to assign a variable and the corresponding intermediate
    // result to the same hard register.
    double nine = (glob - 2.0) * three;
    double ten = (10.0 - glob) * two;
    double eleven = (glob * two) + one;
    double twelve = (glob + 1.0) * two;
    double thirteen = (2. * two) + 9.;
    double fourteen = (3. + four) * 2.;

    // Save to global variables to validate later
    glob9 = nine;
    glob10 = ten;
    glob11 = eleven;
    glob12 = twelve;
    glob13 = thirteen;
    glob14 = fourteen;

    // Validate arguments
    check_14_doubles(one, two, three, four, five, six, seven, eight, 9.0, 10.0,
                     11.0, 12.0, 13.0, 14.0, 1.0);

    // Validate globals
    check_one_double(glob9, 9.0);
    check_one_double(glob10, 10.0);
    check_one_double(glob11, 11.0);
    check_one_double(glob12, 12.0);
    check_one_double(glob13, 13.0);
    check_one_double(glob14, 14.0);
    return 0;
}