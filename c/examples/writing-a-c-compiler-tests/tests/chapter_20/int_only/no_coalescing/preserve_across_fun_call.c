/* Make sure variables that must be preserved across function calls are placed
 * in callee-saved registers. This test is only guaranteed to work as intended
 * once we implement register coalescing;  before then, it's possible (though
 * unlikely) that we'll put each pseudo in an appropriate hard register by
 * accident.
 */

#include "../util.h"

int glob1 = 1;
int glob2 = 2;
int glob3 = 3;
int glob4 = 4;
int glob5 = 5;

int callee(int a, int b, int c, int d, int e) {
    glob1 = -a;
    glob2 = -b;
    glob3 = -c;
    glob4 = -d;
    glob5 = -e;
    // call a function that takes 6 arguments to make sure
    // we clobber every caller-saved register
    check_5_ints(1, 2, 3, 4, 5, 1);
    return 0;
}

int target(void) {
    // Define some variables that must persist across function call.
    int a = 99 * glob1;               // 99
    int b = 200 / glob2;              // 100
    int c = glob3 ? 104 - glob3 : 0;  // 101
    int d = c + (glob4 || glob1);     // 102
    int e = 108 - glob5;              // 103

    // Note that even once we have register coalescing, we won't coalesce
    // any of these arguments into parameter passing registers,
    // because they interfere with them.
    callee(a, b, c, d, e);

    // validate global variables, which were updated by callee
    check_one_int(glob1, -99);
    check_one_int(glob2, -100);
    check_one_int(glob3, -101);
    check_one_int(glob4, -102);
    check_one_int(glob5, -103);

    // define 5 other variables that conflict with a-e but don't need to be
    // preserved across function calls, in order to increase
    // register pressure and decrease the likelihood that we put a-e
    // in the correct registers by accident, even before coalescing
    int f = a - 100;
    int g = b - 100;
    int h = c - 100;
    int i = d - 100;
    int j = e - 100;

    // same f-h to global variables to validate later (don't use check_5_ints
    // b/c that would make param-passing registers interfere with a-e,
    // preventing us from coalescing them, even if we didn't recognize
    // that all function calls clobber those registers)
    glob1 = f;
    glob2 = g;
    glob3 = h;
    glob4 = i;
    glob5 = j;

    // validate a-e to make sure they haven't been clobbered by
    // any intervening function calls
    check_one_int(a, 99);
    check_one_int(b, 100);
    check_one_int(c, 101);
    check_one_int(d, 102);
    check_one_int(e, 103);

    // validate globals w/ values of f-h
    check_5_ints(glob1, glob2, glob3, glob4, glob5, -1);

    return 0;  // success
}