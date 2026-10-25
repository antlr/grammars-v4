/* Make sure we use optimistic coloring; that is, not all spill candidates are
 * actually spilled. When we color the interference graph, we'll choose 10
 * spill candidates but only spill 5 of them.
 */

#include "../util.h"

int flag = 0;
int result = 0;

// global variables; use these to initialize pseudos while
// preventing copy propagation
int glob0;
int glob1;
int glob2;
int glob3;
int glob4;

// helper functiont set global variables to increasing values starting with
// start
int set_globals(int start) {
    glob0 = start;
    glob1 = start + 1;
    glob2 = start + 2;
    glob3 = start + 3;
    glob4 = start + 4;
    return 0;
}

int target(void) {
    /*
     * We create three cliques of five callee-saved pseudos.
     * The pseudos in the first clique, zero through four, interfere with
     * all the others. The second clique (five through nine) and the third
     * (ten through fourteen) don't interfere with each other.
     * Pseudos five through nine have the lowest spill cost/spill metric,
     * and ten through fourteen have the highest spill cost.
     * During the coloring process, we'll:
     * 1. Prune temporaries that aren't in these cliques.
     * 2. Get stuck b/c each pseudo interferes with at least nine others
     *    plus seven caller-saved hard registers.
     * 3. Prune five through nine as spill candidates because they have
     *    the lowest spill metrics. Each prune reduces the degree of the nodes
     *    in this clique and of zero through four, but not by enough to prune
     *    them, so we stay stuck.
     * 4. Prune zero through four as spill candidates, reducing degree of
     *    remaining nodes to less then 12.
     * 5. Prune nodes ten through thirteen and hard registers.
     * 6. Color nodes ten through thirteen and hard registers.
     * 7. Spill nodes zero through four because they interfere with ten through thirteen
     *    and all caller-saved hard registers, leaving no colors
     *    available.
     * 8. Color nodes five through nine; although they were spill candidates,
     *    they don't interfere with five through nine, so all callee-saved hard
     *    registers are available.
     * 9. Color any remaining temporaries.
     *
     * TLDR: We choose zero through four and five through nine as spill
     * candidates but only actually spill zero through four.
     */



    // define zero through four, initialize them to 0 through 4
    set_globals(0);
    int zero = glob0;
    int one = glob1;
    int two = glob2;
    int three = glob3;
    int four = glob4;

    // define five through nine, initialize to 5 through 9
    set_globals(5);
    int five = glob0;
    int six = glob1;
    int seven = glob2;
    int eight = glob3;
    int nine = glob4;

    // use each variable once
    check_5_ints(zero, one, two, three, four, 0);
    check_5_ints(five, six, seven, eight, nine, 5);

    // define ten through fourteen, initialize to 10 through 14
    set_globals(10);
    int ten = glob0;
    int eleven = glob1;
    int twelve = glob2;
    int thirteen = glob3;
    int fourteen = glob4;

    // check zero-four first to force fourteen to be callee-saved
    check_5_ints(zero, one, two, three, four, 0);

    // use ten through fourteen a bunch of times to increase spill cost
    check_5_ints(ten, eleven, twelve, thirteen, fourteen, 10);
    check_5_ints(ten, eleven, twelve, thirteen, fourteen, 10);
    check_5_ints(ten, eleven, twelve, thirteen, fourteen, 10);
    check_5_ints(ten, eleven, twelve, thirteen, fourteen, 10);

    // use zero through four one more time so its spill cost is higher than
    // five through nine
    check_5_ints(zero - 3, one - 3, two - 3, three - 3, four - 3, -3);

    return 0;
}
