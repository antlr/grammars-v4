/* Using indexed operands of the form (%reg1, %reg2, scale) reads reg1
 * and reg2. The test script just validates this  program's behavior;
 * it doesn't inspect assembly.
 * Note: only works as intended after we've implemented register coalescing.
 * */

#include "../util.h"

int arr[2] = {1, 2};
long arr2[2] = {3, 4};

// global so we can calculate indices without constant folding
int three = 3;

int main(void) {
    // In our first round of coalescing, we'll coalesce ptr into %reg1
    // and one into %reg2. If we don't recognize that reading
    // (%reg1, %reg2, scale) reads reg1, the second coalescing round will
    // coalesce ptr2 into reg1, clobbering ptr.
    // If we don't recognize that reading (%reg1, %reg2, scale) reads reg2,
    // the second coalescing round will coalesce zero into reg2, clobbering one.
    long one = three - 2;
    long zero = three - 3;
    int *ptr = arr;
    long *ptr2 = arr2;

    // This will be something like:
    //   movq %ptr, %r8
    //   movq %one, %r9
    //   leaq (%r8, %r9, 4), %other_ptr
    int *other_ptr = ptr + one;
    // This will be something like:
    //   movq %ptr2, %r8
    //   movq %zero, %r9
    //   leaq (%r8, %r9, 4), %other_ptr2
    long *other_ptr2 = ptr2 + zero;

    check_one_int(*other_ptr, 2);
    check_one_long(*other_ptr2, 3);
    return 0;
}
