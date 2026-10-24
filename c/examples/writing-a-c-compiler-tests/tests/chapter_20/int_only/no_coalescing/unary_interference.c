/* Test that negl y (or similar) causes interference b/t y and anything else
 * currently live. Just test for correctness, don't inspect assembly.
 * Only guaranteed to work as intended after implementing register coalescing.
 * */

#include "../util.h" // declares check_* and id functions

int main(void) {
    // negation
    int a = id(100);
    /* movl    %a, %b # if we don't recognize a and b interfere,
     *                # we'll coalesce them
     * negl    %b     # creates interference b/c a is still live after
     */
    int b = -a;

    // force a and b to be callee-saved so they don't get
    // coalesced into caller-saved hard regs (which might prevent
    // them from being coalesced together even if we don't
    // recognize that they interfere)
    check_one_int(-1, -1);

    // validate a and b, making them both live
    check_one_int(a, 100);
    check_one_int(b, -100);

    // bitwies complement
    int c = id(200);
    /* movl    %c, %d # if we don't recognize c and d interfere,
     *                # we'll coalesce them
     * negl    %d     # creates interference b/c c is still live after
     */
    int d = ~c;

    // force c and d to be callee-saved so they don't get
    // coalesced into caller-saved hard regs (which might prevent
    // them from being coalesced together even if we don't
    // recognize that they interfere)
    check_one_int(0, 0);

    // validate c and d, making them both live
    check_one_int(c, 200);
    check_one_int(d, -201);
}
