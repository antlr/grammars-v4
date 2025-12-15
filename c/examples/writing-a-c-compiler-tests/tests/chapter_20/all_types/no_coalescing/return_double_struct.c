/* Test that we recognize EAX is not live at exit when we return
 * a struct in XMM registers. If we think EAX is live we won't be able
 * to allocate pseudoregisters to it, which will lead to spills.
 * The test script validates that the assembly program
 * doesn't use any memory access instructions except to populate and
 * transfer the return value struct. There should be at most eight
 * memory access instructions: for each of this function's two return
 * statements (success and failure), two instructions populate the return
 * value struct and two move it into the XMM0/XMM1 registers.
 * The target function, defined in tests/chapter_20/helper_libs/return_double_struct_lib.c,
 * calls return_double and validates the result.
 *
 * This test program is generated from templates/chapter_20_templates/twelve_pseudos_interfere.c.jinja.
 */
struct s {
    double d1;
    double d2;
};
int global_one = 1;  // to prevent constant-folding

struct s return_struct(void) {
    // create a clique of 12 pseudos that interfere
    // we can color all of them w/out spilling anything

    int one = 2 - global_one;
    int two = one + one;
    int three = 2 + one;
    int four = two * two;
    int five = 6 - one;
    int six = two * three;
    int seven = one + 6;
    int eight = two * 4;
    int nine = three * three;
    int ten = four + six;
    int eleven = 16 - five;
    int twelve = six + six;

    // validate them
    if (one == 1 && two == 2 && three == 3 && four == 4 && five == 5 &&
        six == 6 && seven == 7 && eight == 8 && nine == 9 && ten == 10 &&
        eleven == 11 && twelve == 12) {
        struct s retval = {0.0, 200.0};
        return retval; // success
    } else {
        struct s retval = {1.0, -1.0};
        return retval; // fail
    }
}