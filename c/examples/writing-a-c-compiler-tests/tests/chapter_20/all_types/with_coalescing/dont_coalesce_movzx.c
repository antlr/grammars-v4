/* When converting uint to double, make sure we use MovZeroExtend rather than
 * mov to convert uint to long before converting to double, and that
 * we don't coalesce the MovZeroExtend instruction. Just validate
 * behavior, don't inspect assembly.
 * */

#include "../util.h"

long one = 1l;

int main(void) {
    // Assembly will be:
    // movq   %one, %tmp1
    // negq   %tmp1
    // movq   %tmp1, %neg1
    // movl   %neg1, %tmp2
    // movl   %tmp2, %uint_max
    // movzx  %uint_max, %r
    // cvtsi2sdq %r, %d
    // ...
    // where %r is some hard register.
    // If we use a mov instruction instead of movzx,
    // we'll coalesce tmp1, neg1, tmp2, and uint_max into r,
    // and will end up converting -1 to a double instead of 4294967295u

    long neg1 = -one;
    unsigned int uint_max = (unsigned int)neg1;
    double d = (double)uint_max;
    check_one_double(d, 4294967295.0);
    return 0;
}
