/* Make sure we recognize that XMM0 is live at exit.
 * Don't inspect assembly; just validate the program's behavior.
 * Note: only works as intended once we've implemented register coalescing.
 *
 * This test program is generated from templates/chapter_20_templates/reg_live_at_exit.c.jinja
 * */

#include "../util.h"

double glob = 10.0;
double glob2 = 0.0;

// The first round of coalescing will coalesce x into XMM0.
// Then, if we don't realize that XMM0 is live at exit, we'll
// coalesce the temporary that holds x + glob into XMM0, clobbering x.

double target(void) {
    double x = glob + 1.0;  // 11.0
    glob2 = x + glob;
    return x;
}

int main(void) {
    double retval = target();
    check_one_double(retval, 11.0);
    check_one_double(glob2, 21.0);
    return 0;
}