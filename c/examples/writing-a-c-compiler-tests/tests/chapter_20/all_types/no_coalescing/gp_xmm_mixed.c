/* Test case in a program with a mix of integer and floating-point
 * pseudoregisters; make sure we can allocate all of them.
 * Test script validates that there are no spills.
 */

#include "../util.h"

int target(int one, int two, double one_d, double two_d, int three,
           double three_d) {
    // Define ints 4-8 (all callee-saved) and doubles 10-23 and validate them
    long four = two * two;
    long five = three + two_d;

    double ten_d = three * two_d + four;
    double eleven_d = ten_d + one;

    long six = three * two_d;
    long seven = four + 3;

    double twelve_d = six * two_d;
    double thirteen_d = 14.0 - one_d;
    double fourteen_d = seven * two;
    double fifteen_d = twelve_d + three;
    double sixteen_d = four * four;
    double seventeen_d = ten_d + seven;
    double eighteen_d = three_d * six;

    unsigned long eight = four * two;

    double nineteen_d = 20 - one;
    double twenty_d = four * five;
    double twenty_one_d = three * 7;
    double twenty_two_d = eleven_d * 2;
    double twenty_three_d = ten_d + thirteen_d;

    check_14_doubles(ten_d, eleven_d, twelve_d, thirteen_d, fourteen_d,
                     fifteen_d, sixteen_d, seventeen_d, eighteen_d, nineteen_d,
                     twenty_d, twenty_one_d, twenty_two_d, twenty_three_d,
                     10.0);
    check_5_ints(four, five, six, seven, eight, 4);
    return 0;
}