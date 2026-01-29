/* Make sure we recognize that a function uses some parameter-passing
 * XMM registers, determined by its declaration. Don't inspect assembly,
 * just validate behavior.
 * NOTE: only works as intended after we've implemented register coalescing.
 *
 * This test program is generated from templates/chapter_20_templates/funcall_generates_args.c.jinja
 * */

#include "../util.h"

// defined in tests/chapter_20/helper_libs/funcall_generates_args_lib.c,
// exits early with return code -1 if a and b don't have
// the correct values
int use_dbls(double a, double b);

double glob = 10.0;
double x = 0.0;
double y = 0.0;
int target(void) {
    double a = glob + 1.0;
    double b = glob + 2.0;
    // We'll coalesce a and b with XMM0/XMM1 because they're copied into those
    // registers. If we don't recognize that XMM0/XMM1 are live when we call
    // use_dbls, we'll coalesce the temporaries that hold a * glob and b * glob
    // with XMM0/XMM1 too, since we'll generate the following assembly:
    // movsd  %a, %tmp
    // mulsd  %glob, %tmp
    // movsd  %tmp, %x
    // and similar for y/b
    x = a * glob;
    y = b * glob;
    // validate a and b
    use_dbls(a, b);
    // validate x and y
    check_one_double(x, 110.0);
    check_one_double(y, 120.0);
    return 0;
}

int main(void) {
    return target();
}