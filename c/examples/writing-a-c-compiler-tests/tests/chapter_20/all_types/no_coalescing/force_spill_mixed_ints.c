/* Test that we can handle spilling correctly.
 * We have to spill one pseudo. The test script will validate that
 * we spill only one and it's the cheapest one.
 * Note that this isn't a foolproof test of spill cost calculation;
 * because of optimistic coloring, we might end up spilling should_spill
 * even if it's not the first spill candidate we select.
 * This test program is generated from templates/chapter_20_templates/force_spill.c.jinja
 * */

#include "../util.h"

unsigned int glob_three = 3;
long glob_11 = 11l;
double glob_12 = 12.0;
long glob_23 = 23l;
double glob_24 = 24.0;

int target(void) {
    // This is our spill candidate: it has the highest degree and is
    // used only once.
    long should_spill = glob_three + 3;

    // create 12 pseudos that all interfere w/ should_spill and each other;
    // this forces a spill, since only 12 hard registers are available
    unsigned int one = glob_three - 2;
    long two = one + one;
    unsigned long three = 2 + one;
    char four = two * two;
    signed char five = 6 - one;
    int six = two * three;
    unsigned char seven = one + 6;
    long eight = two * 4;
    unsigned long nine = three * three;
    char ten = four + six;
    long* eleven = &glob_11;
    double* twelve = &glob_12;

    // validate one through twelve
    // (this makes them all live at this point)
    check_12_vals(one, two, three, four, five, six, seven, eight, nine, ten,
                  eleven, twelve, 1);
    // create another clique of twelve pseudos that interfere with each other
    // and should_spill, so should_spill will have more conflicts than other
    // pseudoregisters
    unsigned int thirteen = 10 + glob_three;
    long fourteen = thirteen + 1;
    unsigned long fifteen = 28 - thirteen;
    char sixteen = fourteen + 2;
    signed char seventeen = 4 + thirteen;
    int eighteen = 32 - fourteen;
    unsigned char nineteen = 35 - sixteen;
    unsigned int twenty = fifteen + 5;
    long twenty_one = thirteen * 2 - 5;
    unsigned long twenty_two = fifteen + 7;
    long* twenty_three = &glob_23;
    double* twenty_four = &glob_24;

    // validate thirteen through twenty-four
    // (this makes them all live at this point)
    check_12_vals(thirteen, fourteen, fifteen, sixteen, seventeen, eighteen,
                  nineteen, twenty, twenty_one, twenty_two, twenty_three,
                  twenty_four, 13);

    if (should_spill != 6) {
        return -1;  // fail
    }

    return 0;  // success
}