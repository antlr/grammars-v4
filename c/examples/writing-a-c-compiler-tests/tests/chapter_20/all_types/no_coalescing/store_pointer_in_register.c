/* Test that writing through a pointer doesn't update the register holding
 * the pointer. e.g.
 * movq %ptr, %r
 * mov %src, (%r)
 * ... use ptr
 * does not create conflict b/t %ptr and %r
 * Look for: no spills
 */

#include "../util.h"

int glob1 = 1;
int glob2 = 2;
int glob3 = 3;
int glob4 = 4;
int glob5 = 5;
int glob6 = 6;
int glob7 = 7;

int flag = 1;

int* store_a;

int target(void) {
    // Define 5 variable that take up callee-saved registers
    int callee_saved1 = glob1;
    int callee_saved2 = glob2;
    int callee_saved3 = glob3;
    int callee_saved4 = glob4;
    int callee_saved5 = glob5;

    // call a function to force these to be callee-saved
    check_5_ints(1, 2, 3, 4, 5, 1);
    // Create a clique of 7 local variables that interfere.
    // Each one is a pointer that we write through, which remains live
    // afterwards. As long as we recognize that writing through a pointer
    // doesn't update that pointer, we can assign each of these to a
    // caller-saved register and avoid spills.
    int* a;
    int* b;
    int* c;
    int* d;
    int* e;
    int* f;
    int* g;
    // use flag to avoid copy prop
    if (flag) {
        a = &glob1;
        *a = 2;
        b = &glob2;
        *b = 4;
        c = &glob3;
        *c = 6;
        d = &glob4;
        *d = 8;
        e = &glob5;
        *e = 10;
        f = &glob6;
        *f = 12;
        g = &glob7;
        // every tmp will conflict with the register we load pointers into,
        // except g (b/c g isn't live while we load earlier pointers into that
        // register)
        *g = 14;
        // store a to a global variable so that all regs conflict but we'll have
        // lower register pressure later when we do comparisons
        store_a = a;

    } else {
        a = 0;
        b = 0;
        c = 0;
        d = 0;
        e = 0;
        f = 0;
        g = 0;
    }

    if (b != &glob2 || c != &glob3 || d != &glob4 || e != &glob5 ||
        f != &glob6 || g != &glob7) {
        return 1;
    }
    if (glob1 != 2 || glob2 != 4 || glob3 != 6 || glob4 != 8 || glob5 != 10 ||
        glob6 != 12 || glob7 != 14) {
        return 2;
    }
    if (store_a != &glob1) {
        return 3;
    }

    if (callee_saved1 != 1 || callee_saved2 != 2 || callee_saved3 != 3 ||
        callee_saved4 != 4 || callee_saved5 != 5) {
        return 4;
    }
    return 0;
}
