/* Make sure that when we apply the Briggs test to floating-point
 * registers, we consider a node significant only if it has 14 or more
 * neighbors (not 12 or more as for integer registers).
 * The test script validates that there are no spills and no movsd instructions
 * where both operands are XMM registers.
 * */

#include "../util.h"

// store some initial values in global variables
// to prevent constant folding
double glob0 = 0;
double glob1 = 1.;
double glob2 = 2.;
double glob10 = 10.;

// we'll copy pseudos to these, then validate these with check_one_double
double glob_zero;
double glob_one;
double glob_two;
double glob_three;
double glob_four;
double glob_five;
double glob_six;
double glob_seven;
double glob_eight;
double glob_nine;
double glob_ten;
double glob_eleven;
double glob_twelve;
double glob_thirteen;
double glob_fourteen;

// use this to prevent copy prop into check_one_double calls
void incr_glob1(void) {
    glob1 = glob1 + 1;
}

int target(void) {
    // Create thirteen pseudos that interfere with each other (one-thirteen)
    // plus two pseudos (zero and fourteen) that interfere with those
    // thirteen pseudos but not with each other.

    // For each of one-thirteen, calculating the initial value requires
    // an intermediate result, which we can coalesce with the corresponding
    // pseudoregister if we've implemented the Briggs test correctly.

    // Only fourteen nodes, zero-thirteen, have significant degree, so nobody
    // can have more than 13 neighbors with significant degree, so all our
    // coalescing candidates will fail the Briggs test. (zero interferes with
    // one through thirteen plus all the intermediate values; one-twelve
    // interfere with zero, each other, and varying numbers of intermediate
    // values; thirteen interferes with zero through twelve plus fourteen.
    // fourteen does _not_ have significant degree).

    // If our definition of 'significant degree' is 12 or more neighbors,
    // we'll think fourteen has significant degree and all our coalescing
    // candidates will fail the Briggs test.

    double zero = glob0 * 10.;
    double one = glob10 / 2. - 4.;
    double two = glob10 / 2. - 3.;
    double three = glob2 * 2. - 1;
    double four = (6. - glob2) * glob1;
    double five = (glob10 / 2.) * glob1;
    double six = (glob10 + 2.) / 2;
    double seven = 3. * glob2 + 1.;
    double eight = glob2 * glob2 * 2.;
    double nine = (glob1 + glob2) * 3.;
    double ten = (glob2 + 3.) * 2.;
    double eleven = (glob10 + 1.) * glob1;
    double twelve = (glob1 + glob2) * 4.;
    double thirteen = (glob2 * 3.) + 7.;
    glob_zero = zero;  // save zero here so it doesn't interfere with fourteen
    double fourteen = glob2 * 7.;

    // Save one-fourteen to global variables (so we can then
    // validate those global variables without causing any new interference
    // with pseudos)
    glob_one = one;
    glob_two = two;
    glob_three = three;
    glob_four = four;
    glob_five = five;
    glob_six = six;
    glob_seven = seven;
    glob_eight = eight;
    glob_nine = nine;
    glob_ten = ten;
    glob_eleven = eleven;
    glob_twelve = twelve;
    glob_thirteen = thirteen;
    glob_fourteen = fourteen;

    //  call a function to prevent copy prop
    incr_glob1();

    // validate the global variables
    check_one_double(glob_zero, 0.);
    check_one_double(glob_one, 1.0);
    check_one_double(glob_two, 2.0);
    check_one_double(glob_three, 3.0);
    check_one_double(glob_four, 4.0);
    check_one_double(glob_five, 5.0);
    check_one_double(glob_six, 6.0);
    check_one_double(glob_seven, 7.0);
    check_one_double(glob_eight, 8.0);
    check_one_double(glob_nine, 9.0);
    check_one_double(glob_ten, 10.0);
    check_one_double(glob_eleven, 11.0);
    check_one_double(glob_twelve, 12.0);
    check_one_double(glob_thirteen, 13.0);
    check_one_double(glob_fourteen, 14.0);

    // make sure we actually called incr_glob1
    check_one_double(glob1, 2.);

    return 0;
}