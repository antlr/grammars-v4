/* Test that we can handle spilling correctly.
 * We have to spill one pseudo. The test script will validate that
 * we spill only one and it's the cheapest one.
 * Note that this isn't a foolproof test of spill cost calculation;
 * because of optimistic coloring, we might end up spilling should_spill
 * even if it's not the first spill candidate we select.
 * This test program is generated from templates/chapter_20_templates/force_spill.c.jinja
 * */

#include "../util.h"

int glob_three = 3;

int target(void) {
    // This is our spill candidate: it has the highest degree and is
    // used only once.
    int should_spill = glob_three + 3;

    // create 12 pseudos that all interfere w/ should_spill and each other;
    // this forces a spill, since only 12 hard registers are available
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
    // and should_spill, so should_spill will have more conflicts than other
    // pseudoregisters
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

    if (should_spill != 6) {
        return -1;  // fail
    }

    return 0;  // success
}