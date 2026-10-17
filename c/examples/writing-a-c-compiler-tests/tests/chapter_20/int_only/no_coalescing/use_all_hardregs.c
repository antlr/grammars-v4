/* Make sure we use all hardregs rather than spill;
 * Create 12 pseudos that all interfere with each other
 * and make sure we assign all of them to hardregs
 * This test program is generated from templates/chapter_20_templates/use_all_hardregs.c.jinja
 * */
#include "../util.h"

int global_one = 1;  // to prevent constant-folding

int target(void) {
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

    // validate one through twelve
    // (this makes them all live at this point)
    check_12_ints(one, two, three, four, five, six, seven, eight, nine, ten,
                  eleven, twelve, 1);
    return 0;  // success
}