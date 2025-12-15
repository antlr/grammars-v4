/* Make sure we use k = 14 instead of k = 12 when applying the George test to
 * floating-point registers. The test script validates that there are no spills
 * and no moves between XMM registers.
 */

#include "../util.h"

// use these to create non-const-foldable initial values
double glob1 = 1.;
double glob2 = 2.;
double glob10 = 10.;

// If we've implemented the George test correctly, we'll be able to coalesce
// one-eight into their respective parameter-passing registers. Then for pseudos
// nine-fourteen, we'll able to use the Briggs test to coalesce each
// intermediate result into the corresponding pseudoregister.

// Pseudoregisters one through fourteen all interfere with each other and with
// XMM0-XMM7 (except that each parameter doesn't interfere with the hard
// register it's passed in). Each of these pseudos also interferes with any
// intermediate results calculated after it (e.g. one-thirteen all interfere
// with the result of glob2 * 6., which we use to define fourteen, but fourteen
// doesn't interfere with earlier intermediate results.) None of the
// intermediate results have significant degree. Coalescing any parameter p into
// the corresponding parameter-passing reg r passes the Briggs test because each
// neighbor of p is either:
// - a hard register, which conflicts with r
// - one of pseudos one-fourteen, which conflicts with r
// - an intermediate result, which doesn't have significant degree.

// The last couple of intermediate results (glob2 * 3. and glob2 * 6.) have
// degree >= 12; if we think they have significant degree, they'll prevent
// pseudos one-eight from passing the George test.
int target(double one, double two, double three, double four, double five,
           double six, double seven, double eight) {
    // copypasta from briggs_xmm_k_value.c
    double nine = (glob1 + glob2) * 3.;
    double ten = (glob2 + 3.) * 2.;
    double eleven = (glob10 + 1.) * glob1;
    double twelve = (glob1 + glob2) * 4.;
    double thirteen = (glob2 * 3.) + 7.;
    double fourteen = glob2 * 6. + 2.;

    check_14_doubles(one, two, three, four, five, six, seven, eight, nine, ten,
                     eleven, twelve, thirteen, fourteen, 1.);

    return 0;
}
