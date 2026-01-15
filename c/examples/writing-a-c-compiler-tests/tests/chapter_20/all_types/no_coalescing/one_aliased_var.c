/* Make sure that one aliased pseudoregister doesn't prevent us from allocating
 * pseudos for other, non-aliased pseudoregisters.
 * We have to put arguments one and one_d on the stack because they're aliased,
 * but should be able to store all other pseudos in registers.
 */

#include "../util.h"

void increment(int *ptr) {
    *ptr = *ptr + 1;
    return;
}

double deref(double *ptr) {
    return *ptr;
}

// This should contain at most three memory access instructions (not counting
// push/pop/lea): moving one from EDI to the stack, moving one_d from XMM0 to
// the stack, and moving one back into the EDI register before calling
// check_one_int
int target(int one, int two, int three, double one_d) {
    int *ptr = &one;
    double *d_ptr = &one_d;
    check_one_double(deref(d_ptr), 1.0);
    increment(ptr);  // increment one
    long five = two + three;
    check_one_int(one, 2);
    check_one_long(five, 5l);
    return 0;
}