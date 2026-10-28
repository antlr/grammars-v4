/* Make sure we recognize that div makes EAX live.
 * Don't inspect assembly, just validate behavior
 * NOTE: only works as intended once we implement coalescing.
 *
 * This test program is generated from templates/chapter_20_templates/division_uses_ax.c.jinja
 */

#include "../util.h"

int main(void) {
    // we'll coalesce this into EAX because it's a function's return value,
    // and because we move it into EAX to perform modulo later
    unsigned int coalesce_into_eax = unsigned_id(10);

    // After first round of coalescing we'll have:
    // movl %eax, %sum
    // addl $4, %sum
    // If we don't know EAX is live, we'll coalesce sum into EAX in second
    // round of coalescing.
    unsigned int sum = coalesce_into_eax + 4;

    // validate sum; don't use check_one_uint here because we don't want
    // to coalesce this into a parameter-passing register or force
    // coalesce_into_eax to be callee_saved
    if (sum != 14) {
        return -1;
    }

    // To recognize that coalesce_into_eax is still live here,
    // we need to know that div uses EAX. Otherwise we'll have
    // clobbered EAX w/ sum and this will give us the wrong answer.
    // movl %coalesce_into_eax, %eax
    // movl $0, %edx
    // divl $10
    // movl %edx, %rem
    unsigned int rem = coalesce_into_eax % 10;

    // validate rem
    check_one_uint(rem, 0);

    return 0;  // success
}