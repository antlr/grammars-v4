/* Test that we recognize that a binary SSE instruction uses its source and
 * destination; e.g. addsd %a, %b makes a and b live. Don't inspect assembly,
 * just validate behavior.
 * NOTE: only works as intended after we've implemented register coalescing.
 *
 * This test program is generated from templates/chapter_20_templates/bin_uses_operands.c.jinja
 */

#include "../util.h"

// recognize that addsd uses its source
double src_test(double arg) {
    // this becomes:
    // movsd .Lconst_5(%rip), %x
    // addsd %arg, %x
    // if we don't recognize that addsd makes arg live, we'll coalesce both
    // arg and x into XMM0, and check_one_double will fail
    double x = 5 + arg;
    check_one_double(x, 6.0);
    return 0;
}

double glob = 1;
double glob2;
int flag = 1;
// recognize that divsd uses its destination
int dst_test(void) {
    double a = dbl_id(100.0);

    // wrap this in if statement so we can't copy prop temporary values
    // into check_one_double calls below
    if (flag) {
        // this addition becomes:
        // movsd %a, %tmp
        // addsd %glob, %tmp
        // movsd %tmp, %glob2
        // so we'll coalesce a & tmp unless we recognize that a is still live,
        // which requires us to realize that the divsd instruction below
        // doesn't kill it
        glob2 = a + glob;

        // first round of coalescing will coalesce a with temporary result of
        // a / 2.0, so this will be
        // divsd .Lconst_2(%rip), %a.0
        // so we need to recognize that this divsd instruction uses a
        a = a / 2.0;
    }

    check_one_double(a, 50.0);
    check_one_double(glob2, 101.0);
    return 0;
}

int main(void) {
    src_test(1);
    dst_test();
}