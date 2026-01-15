/* Test that we don't coalesce a pseudo into a hardreg if they fail the George
 * and Briggs tests - specifically when they're connected by a mov of the form
 * movl %hardreg, %pseudo. (For the equivalent test with movl %pseudo, %hardreg,
 * see george_dont_coalesce_2.c) Coalescing registers that fail the George test
 * here would force us to spill; we inspect the assembly for target to make
 * sure there are no spills.
 * */

#include "../util.h"

int glob = 1;

// a-f have values 1-6
int target(int a, int b, int c, int d, int e, int f) {
    // a-f interfere with g-l, g-l interfere wih m-q, and m-q interfere with
    // all the param-passing registers. The only way to avoid spills is to put
    // at least five of a-f in non-param-passing registers, so we can put at
    // least five of pseudos g-l in param-passing registers, leaving five
    // non-param-passing registers available for m-q. This means we can't
    // coalesce all of a-f into parameter-passing registers.

    // define g-l (values 7-12)
    int g = a + f;  // 7
    int h = b * d;  // 8
    int i = c * c;  // 9
    int j = d + f;  // 10
    int k = e + f;  // 11
    int l = f * b;  // 12

    // define m-q; use a-f in initializer for m to make them interfere with g-l
    int m = (a + b + c + d + e + f) - 7;  // 14
    int n = g + h;                        // 15
    int o = i + 7;                        // 16
    int p = j * 2 - 3;                    // 17
    int q = k + g;                        // 18
    // this makes m-q conflict w/ all param-passing registers
    check_12_ints(g, h, i, j, k, l, 13, m, n, o, p, q, 7);
    return 0;
}
