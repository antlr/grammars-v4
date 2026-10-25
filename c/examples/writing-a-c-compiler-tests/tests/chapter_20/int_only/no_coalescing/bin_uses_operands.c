/* Test that we recognize that a binary instruction uses its source and
 * destination; e.g. addl %a, %b makes a and b live. Don't inspect assembly,
 * just validate behavior.
 * NOTE: only works as intended after we've implemented register coalescing.
 *
 * This test program is generated from templates/chapter_20_templates/bin_uses_operands.c.jinja
 */

#include "../util.h"

// recognize that addl uses its source
int src_test(int arg) {
    // this becomes:
    // movl $5, %x
    // addl %arg, %x
    // if we don't recognize that addl makes arg live, we'll coalesce both
    // arg and x into EDI, and check_one_int will fail
    int x = 5 + arg;
    check_one_int(x, 6);
    return 0;
}

int glob = 1;
int glob2;
int flag = 1;
// recognize that sub uses its destination
int dst_test(void) {
    int a = id(100);

    // wrap this in if statement so we can't copy prop temporary values
    // into check_one_int calls below
    if (flag) {
        // this addition becomes:
        // movl %a, %tmp
        // addl %glob, %tmp
        // movl %tmp, %glob2
        // so we'll coalesce a & tmp unless we recognize that a is still live,
        // which requires us to realize that the subl instruction below
        // doesn't kill it
        glob2 = a + glob;

        // first round of coalescing will coalesce a with temporary result of
        // a - 1, so this will be
        // subl $1, %a.0
        // so we need to recognize that this subl instruction uses a
        a = a - 1;
    }

    check_one_int(a, 99);
    check_one_int(glob2, 101);
    return 0;
}

int main(void) {
    src_test(1);
    dst_test();
}