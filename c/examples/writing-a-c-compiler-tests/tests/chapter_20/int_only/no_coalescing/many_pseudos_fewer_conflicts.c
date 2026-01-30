/* Test that we can allocate every pseudo without spilling
 * if there are more pseudoregisters than hard registers but they don't all
 * conflict
 */

#include "../util.h"

// define a function that returns a value
// so we can't just constant fold everything away
int return_five(void) {
    return 5;
}

int target(int one, int two, int three) {
    // define and use some variables
    int sum = one + three;
    int product = sum * three;
    int diff = product - (three + two);
    check_one_int(sum, 4);
    check_one_int(product, 12);
    check_one_int(diff, 7);

    // define and use more variables in two branches;
    // shouldn't interfere with earlier variables,
    // and variables from one branch shouldn't interfere
    // with variables in the other
    for (int i = 0; i < 2; i = i + 1) {
        if (i % 2) {
            int five = return_five();
            int quotient = 25 / five;                        // 5
            int remainder = 27 % five;                       // 2
            int complex = (quotient + 3) * (remainder + 4);  // 48
            check_one_int(quotient, 5);
            check_one_int(remainder, 2);
            check_one_int(complex, 48);
        } else {
            int hundred = return_five() * 20;
            int ninety = hundred - 10;
            int seventy = i % 2 ? 0 : hundred / 2 + 20;
            int negative_one_forty_five = (-ninety / 2) - hundred;
            check_one_int(hundred, 100);
            check_one_int(ninety, 90);
            check_one_int(seventy, 70);
            check_one_int(negative_one_forty_five, -145);
        }
    }

    // one more block of variables
    int negative_six = ~return_five();
    int negative_five = -11 - negative_six;
    int negative_four = return_five() - 9;
    int negative_three = negative_six / 2;
    int negative_two = negative_six / 3;

    // validate these five variables all at once
    check_5_ints(negative_six, negative_five, negative_four, negative_three,
                 negative_two, -6);

    return 0;  // success
}