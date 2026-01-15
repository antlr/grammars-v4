/* Make sure we recognize that cmp uses both of its operands.
 * Don't inspect assembly; just validate the program's behavior.
 * Note: this only works as intended after we've implemented register
 * coalescing.
 * */
#include "../util.h"

int glob = 10;
int glob2 = 20;

int main(void) {
    int a = glob + 5; // 15
    int b = glob2 - 5; // 15

    // we'll coalesce a and b
    // into tmps that hold these sums
    // if we don't think a and b are live afterward
    glob = a + glob; // 25
    glob2 = b + glob2; // 35

    if (a != b) {
        return -1;
    }
    // validate glob and glob2
    check_one_int(glob, 25);
    check_one_int(glob2, 35);
    return 0;
}