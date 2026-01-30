/* Validate that movl x, y doesn't cause interference between x and y. The test
 * script will validate that there are no spills; exactly five callee-saved
 * pseudoregisters conflict at any time.
 * */

#include "../util.h"

int glob0 = 0;
int glob1 = 1;
int glob2 = 2;
int glob3 = 3;
int glob4 = 4;
int glob5 = 5;

// use this to force pseudoregs to be callee-saved
int reset_globals(void) {
    glob0 = 0;
    glob1 = 0;
    glob2 = 0;
    glob3 = 0;
    glob4 = 0;
    glob5 = 0;
    return 0;
}

int flag = 1;  // make this static to prevent copy propagation

int target(void) {
    /* define some values - must be in callee-saved regs */
    int a = glob0;  // 0
    int b = glob1;  // 1
    int c = glob2;  // 2
    int d = glob3;  // 3
    int e = glob4;  // 4
    int f;
    int g;
    int h;
    int i;
    int j;
    // put this in conditional so copy prop doesn't get rid of these copies
    if (flag) {
        reset_globals();  // force a-e into callee-saved regs
        f = a;            // now f interferes w/ b, c, d, and e but not a
        check_one_int(a, 0);
        g = b;  // now g interferes w/ c, d, e, f but not a, b
        check_one_int(b, 1);
        h = c;  // h interferes with d, e, f, g, h but not a, b or c
        check_one_int(c, 2);
        i = d;  // i interferes with e, f, g, h but not a, b, c, or d
        check_one_int(d, 3);
        j = e;  // j interferes with f, g, h, i but not a, b, c, d, or e
        check_one_int(e, 4);
    } else {
        e = 0;
        f = 0;
        g = 0;
        h = 0;
        i = 0;
        j = 0;
    }
    // now validate f-j
    check_one_int(f, 0);
    check_one_int(g, 1);
    check_one_int(h, 2);
    check_one_int(i, 3);
    check_one_int(j, 4);

    // and validate global vars, make sure they weren't clobbered
    check_one_int(glob0, 0);
    check_one_int(glob1, 0);
    check_one_int(glob2, 0);
    check_one_int(glob3, 0);
    check_one_int(glob4, 0);

    return 0;
}