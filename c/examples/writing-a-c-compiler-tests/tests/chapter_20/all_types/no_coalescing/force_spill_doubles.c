#include "../util.h"

int glob = 3;
double glob2 = 4.0;

int target(void) {
    /* force spill by creating lots of conflicting pseudos
     * validate that we spill the variable should_spill, which is used least
     * and has highest degree
     * Note: this isn't a good test of spill metric calculation;
     * due to optimistic coloring, we could end up spilling just should_spill
     * even if we end up choosing other nodes as spill candidates first
     */
    double should_spill = (double)glob;
    // all these registers conflict with should_spill and each other
    double one = 4.0 - glob;
    double two = one + one;
    double three = (double)glob;
    double four = two * two;
    double five = glob2 + 1;
    double six = glob * 2;
    double seven = one * one + 6.0;
    double eight = two * 4;
    double nine = three * three;
    double ten = four + six;
    double eleven = 16 - five;
    double twelve = six + six;
    double thirteen = five + eight;
    double fourteen = 21 - seven;

    // validate them
    check_14_doubles(one, two, three, four, five, six, seven, eight, nine, ten,
                     eleven, twelve, thirteen, fourteen, 1.0);

    // make another fourteen pseudos that conflict w/ should_spill and each
    // other
    double fifteen = glob2 * 4.0 - 1;
    double sixteen = glob2 * 4.0;
    double seventeen = fifteen + 2.0;
    double eighteen = 35.0 - seventeen;
    double nineteen = sixteen + glob;
    double twenty = glob2 * 5.0;
    double twenty_one = glob * 7.0;
    double twenty_two = 4.0 + eighteen;
    double twenty_three = nineteen + glob + 1;
    double twenty_four = glob2 + twenty;
    double twenty_five = twenty_one + glob2;
    double twenty_six = twenty_five - nineteen + twenty;
    double twenty_seven = glob * 9.0;
    double twenty_eight = twenty_two + 6;
    check_14_doubles(fifteen, sixteen, seventeen, eighteen, nineteen, twenty,
                     twenty_one, twenty_two, twenty_three, twenty_four,
                     twenty_five, twenty_six, twenty_seven, twenty_eight, 15.0);

    if (should_spill != 3.0) {
        return -1;
    }

    return 0;
}
