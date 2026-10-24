/* Make sure we recognize that at least one of cdq/idiv makes EAX live.
 * (There's no way to test these instructions separately.)
 * Don't inspect assembly, just validate behavior
 * NOTE: only works as intended once we implement coalescing.
 *
 * This test program is generated from templates/chapter_20_templates/division_uses_ax.c.jinja
 */

#include "../util.h"

int main(void) {
    // we'll coalesce this into EAX because it's a function's return value,
    // and because we move it into EAX to perform modulo later
    int coalesce_into_eax = id(10);

    // After first round of coalescing we'll have:
    // movl %eax, %sum
    // addl $4, %sum
    // If we don't know EAX is live, we'll coalesce sum into EAX in second
    // round of coalescing.
    int sum = coalesce_into_eax + 4;

    // validate sum; don't use check_one_int here because we don't want
    // to coalesce this into a parameter-passing register or force
    // coalesce_into_eax to be callee_saved
    if (sum != 14) {
        return -1;
    }

    // To recognize that coalesce_into_eax is still live here,
    // we need to know that cdq/idiv use EAX. Otherwise we'll
    // have clobbered EAX w/ sum and this will give us the wrong answer.
    // movl %coalesce_into_eax, %eax
    // cdq
    // idivl $10
    // movl %edx, %rem
    int rem = coalesce_into_eax % 10;

    // validate rem
    check_one_int(rem, 0);

    return 0;  // success
}