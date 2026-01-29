/* Test that we recognize that a unary instruction uses its operand;
 * i.e. negl %a makes a live. Don't inspect assembly, just validate behavior.
 * NOTE: only works as intended after we've implemented register coalescing.
 * TODO consider using a common template for this and bin_uses_operand
 * */

#include "../util.h"

int glob = 1;

int target(void) {
    int a = 0;

    // wrap this in an if statement so we can't copy prop temporary result of
    // id(100) into uses of a below
    if (id(1)) {
        a = id(100);
    }

    // wrap this in if statement so we can't copy prop temporary values
    // into check_one_int calls below
    if (id(1)) {
        // this addition becomes:
        // movl %a, %tmp
        // addl %glob, %tmp
        // movl %tmp, %glob
        // so we'll coalesce a & tmp unless we recognize that a is still live,
        // which requires us to realize that neg instruction below doesn't kill
        // it
        glob = a + glob;

        // First round of coalescing will coalesce a with temporary
        // variable holding result of negation, so this will be:
        // negl %a
        // so we need to recognize that this negl instruction uses a
        a = -a;
    }
    check_one_int(a, -100);
    check_one_int(glob, 101);
    return 0;
}

int main(void) {
    return target();
}