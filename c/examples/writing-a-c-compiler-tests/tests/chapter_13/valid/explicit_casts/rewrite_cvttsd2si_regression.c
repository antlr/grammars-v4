/* Regression test for rewriting cvttsd2si instructions whose destinations are
 * memory operands. The other test programs for this chapter exercise this
 * rewrite rule until we implement register allocation. But once we implement
 * register allocation, cvttsd2si destination operands in those programs will
 * be allocated to hard registers, not memory.
 *
 * In this program, we include a cvttsd2si whose destination will be spilled
 * during register allocation, so we'll still have test coverage for this
 * rewrite rule when we run the chapter 20 test commands.
 *
 * This test program is generated from templates/pre_ch20_spill_var.c.jinja.
 * */

// for validation
int check_12_ints(int start, int a, int b, int c, int d, int e, int f, int g,
                  int h, int i, int j, int k, int l);

// use a variable with static storage duration in operations below
// so they can't be constant folded
double glob = 5000.;

int main(void) {
    // The cvttsd2sdi operation whose result we want to spill;
    // this is our best spill candidate b/c it has the most conflicts with other
    // pseudos and is tied for fewest uses. NOTE: optimizations must be enabled
    // so we propagate the temporary variable holding the result of this
    // expression instead of copying it into should_spill.
    long should_spill = (long)glob;

    // create 12 pseudos that all interfere w/ cvttsd2si result and each
    // other; this forces a spill, since only 12 hard registers are available
    int one = glob - 4999;
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
    int twelve = six + six;

    // validate one through twelve
    // (this makes them all live at this point)
    check_12_ints(one, two, three, four, five, six, seven, eight, nine, ten,
                  eleven, twelve, 1);
    // create another clique of twelve pseudos that interfere with each other
    // and cvttsd2si result, so cvttsd2si result will have more conflicts than
    // other pseudoregisters
    int thirteen = glob - 4987;
    int fourteen = thirteen + 1;
    int fifteen = 28 - thirteen;
    int sixteen = fourteen + 2;
    int seventeen = 4 + thirteen;
    int eighteen = 32 - fourteen;
    int nineteen = 35 - sixteen;
    int twenty = fifteen + 5;
    int twenty_one = thirteen * 2 - 5;
    int twenty_two = fifteen + 7;
    int twenty_three = 6 + seventeen;
    int twenty_four = thirteen + 11;

    // validate thirteen through twenty-four
    // (this makes them all live at this point)
    check_12_ints(thirteen, fourteen, fifteen, sixteen, seventeen, eighteen,
                  nineteen, twenty, twenty_one, twenty_two, twenty_three,
                  twenty_four, 13);
    // use cvttsd2si result to make it interfere with other pseudos
    // and validate that it wasn't clobbered
    if (should_spill != 5000) {
        return -1;
    }
    return 0;  // success
}

// validate that a == start, b == start + 1, ...l == start + 11
// NOTE: 'start' is the last param because if it were first, every
// arg in the caller would interfere with EDI and we'd have to spill more than
// one pseudo
int check_12_ints(int a, int b, int c, int d, int e, int f, int g, int h, int i,
                  int j, int k, int l, int start) {
    int expected = 0;

    expected = start + 0;
    if (a != expected) {
        return expected;
    }

    expected = start + 1;
    if (b != expected) {
        return expected;
    }

    expected = start + 2;
    if (c != expected) {
        return expected;
    }

    expected = start + 3;
    if (d != expected) {
        return expected;
    }

    expected = start + 4;
    if (e != expected) {
        return expected;
    }

    expected = start + 5;
    if (f != expected) {
        return expected;
    }

    expected = start + 6;
    if (g != expected) {
        return expected;
    }

    expected = start + 7;
    if (h != expected) {
        return expected;
    }

    expected = start + 8;
    if (i != expected) {
        return expected;
    }

    expected = start + 9;
    if (j != expected) {
        return expected;
    }

    expected = start + 10;
    if (k != expected) {
        return expected;
    }

    expected = start + 11;
    if (l != expected) {
        return expected;
    }

    return 0;  // success
}
