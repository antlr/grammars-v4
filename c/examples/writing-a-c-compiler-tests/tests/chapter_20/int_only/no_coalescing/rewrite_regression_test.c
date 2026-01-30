/* This isn't really a test of the register allocator.
 * Verify that we correctly rewrite add/sub/imul instructions with operands in
 * memory. Test programs for earlier chapters exercise these rewrite rules only
 * when register allocation and optimizations are disabled. But once we complete
 * Part III, these are either optimized away entirely in earlier chapters' test
 * programs, or their operands are all hard registers.
 *
 * This test program is generated from templates/chapter_20_templates/rewrite_regression_test.c.jinja
 * */

#include "../util.h"

int glob_three = 3;
int glob_four = 4;

int target(void) {
    // We'll force the results of imul, add, and sub instructions to spill
    // to memory (by making them conflict with more registers and have fewer
    // uses then any other pseudo) to verify that we rewrite them correctly

    // These results will conflict with all other pseudos and only be used once
    // each, so they'll all spill
    int imul_result = glob_three * glob_four;
    int add_result = glob_three + glob_four;
    int sub_result = glob_four - glob_three;

    // create 12 pseudos that all interfere w/ imul, add, and sub results and
    // each other; this forces a spill, since only 12 hard registers are
    // available
    int one = glob_three - 2;
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

    // validate one through twelve
    // (this makes them all live at this point)
    check_12_ints(one, two, three, four, five, six, seven, eight, nine, ten,
                  eleven, twelve, 1);
    // create another clique of twelve pseudos that interfere with each other
    // and imul, add, and sub results, so imul, add, and sub results will have
    // more conflicts than other pseudoregisters
    int thirteen = 10 + glob_three;
    int fourteen = thirteen + 1;
    int fifteen = 28 - thirteen;
    int sixteen = fourteen + 2;
    int seventeen = 4 + thirteen;
    int eighteen = 32 - fourteen;
    int nineteen = 35 - sixteen;
    int twenty = fifteen + 5;
    int twenty_one = thirteen * 2 - 5;
    int twenty_two = fifteen + 7;
    int twenty_three = 6 + seventeen;
    int twenty_four = thirteen + 11;

    // validate thirteen through twenty-four
    // (this makes them all live at this point)
    check_12_ints(thirteen, fourteen, fifteen, sixteen, seventeen, eighteen,
                  nineteen, twenty, twenty_one, twenty_two, twenty_three,
                  twenty_four, 13);

    if (imul_result != 12) {
        return 100;
    }
    if (add_result != 7) {
        return 101;
    }
    if (sub_result != 1) {
        return 102;
    }

    return 0;  // success
}