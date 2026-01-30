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

int glob = 0;

int target(int a) {

    // Make sure we don't coalesce a into EDI; this would fail the George
    // test because a's neighbor, one, has degree k. First we define a clique of
    // 12 registers, one-twelve. one interferes with a and has exactly 12
    // neighbors. Once we prune a, we can prune one, then two-twelve. To avoid
    // spilling, one must go in EDI, because two-twelve all conflict with EDI.
    // If we coalesce a with EDI, we won't be able to prune one and we'll have
    // to spill something.
    // NOTE - some of this is copy/pasted from twelve_regs_conflict.c.jinja;
    // it's just different enough that using the template arguably isn't worth
    // it.

    int one = 2 - a;
    int two = one + one;
    int three = 2 + one;
    int four = two * two;
    int five = 6 - one;
    int six = two * three;
    int seven = one + 6;
    int eight = two * 4;
    int nine = three * three;
    int ten = four + six;
    int eleven = 16 - five;
    int twelve = eleven + one;

    // save one to validate later/make it conflict with all the others
    glob = one;

    // validate others/make them conflict with EDI
    check_12_ints(1, two, three, four, five, six, seven, eight, nine, ten,
                  eleven, twelve, 1);

    // validate one
    check_one_int(glob, 1);
    return 0;
}