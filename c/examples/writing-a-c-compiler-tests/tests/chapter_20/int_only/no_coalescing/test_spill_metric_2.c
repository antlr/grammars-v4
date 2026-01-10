/* Make sure our spill metric factors in degree as well as spill cost.
 * We have two cliques of 5 callee-saved pseudos each, and to_spill interferes
 * with all pseudos in both of them. to_spill has high degree but also a slightly
 * higher spill cost than the other pseudos. If we use spill_cost / degree as
 * our spill metric, we'll only spill to_spill and nothing else. If we only use
 * spill cost, and not degree, we'll spill two pseudos, one from each clique,
 * resulting in more memory accesses overall.
 */

#include "../util.h"

int target(void) {

    // to_spill and a-e form a clique and must go in callee-saved regs
    int to_spill = id(1);
    int a = id(2);
    int b = id(3);
    int c = id(4);
    int d = id(5);
    int e = id(6);

    // validate these and increase their spill cost
    check_one_int(to_spill, 1);
    check_5_ints(a, b, c, d, e, 2);
    check_5_ints(1 + a, 1 + b, 1 + c, 1 + d, 1 + e, 3);
    check_one_int(to_spill, 1);

    // to_spill and f-j form a clique and must go in callee-saved regs
    int f = id(7);
    int g = id(8);
    int h = id(9);
    int i = id(10);
    int j = id(11);

    // validate these and increase their spill cost
    check_5_ints(f, g, h, i, j, 7);
    check_5_ints(1 + f, 1 + g, 1 + h, 1 + i, 1 + j, 8);
    check_one_int(to_spill, 1);
    return 0;
}