/* Check for off-by-one errors in the George test. Make sure we don't coalesce
 * pseudo p into hardreg h if p has a neighbor whose degree is exactly k.
 * The test script validates that there are no spills - we shouldn't need to
 * spill any pseudos, but we'll be forced to spill if we coalesce a pseudo
 * into a hard register when we shouldn't.
 * NOTE: we don't have an equivalent off-by-one test for Briggs b/c it's
 * adequately covered by other tests.
 *
 * This test was generated from
 * templates/chapter_20_templates/george_off_by_one.c.jinja.
 * */

#include "../util.h"

double glob = 0.0;

int target(double a) {

    // Make sure we don't coalesce a into XMM0; this would fail the George
    // test because a's neighbor, one, has degree k. First we define a clique of
    // 14 registers, one-fourteen. one interferes with a and has exactly 14
    // neighbors. Once we prune a, we can prune one, then two-fourteen. To avoid
    // spilling, one must go in XMM0, because two-fourteen all conflict with
    // XMM0. If we coalesce a with XMM0, we won't be able to prune one and we'll
    // have to spill something.
    // NOTE - some of this is copy/pasted from twelve_regs_conflict.c.jinja;
    // it's just different enough that using the template arguably isn't worth
    // it.

    double one = 2.0 - a;
    double two = one + one;
    double three = 2.0 + one;
    double four = two * two;
    double five = 6.0 - one;
    double six = two * three;
    double seven = one + 6.0;
    double eight = two * 4.0;
    double nine = three * three;
    double ten = four + six;
    double eleven = 16.0 - five;
    double twelve = eleven + one;
    double thirteen = five + eight;
    double fourteen = seven * two;

    // save one to validate later/make it conflict with all the others
    glob = one;

    // validate others/make them conflict with XMM0
    check_14_doubles(1, two, three, four, five, six, seven, eight, nine, ten,
                     eleven, twelve, thirteen, fourteen, 1);

    // validate one
    check_one_double(glob, 1.0);
    return 0;
}