/* If we deduplicate floating-point StaticConstant constructs, make sure we
 * distinguish between constants with the same value but different alignments.
 * Specifically, if we've already added an ordinary constant -0.0, and then we
 * need a 16-byte aligned -0.0 to use for negation, don't just reuse the
 * previous 8-byte aligned one. (It's okay to either keep them as separate
 * constants, or merge them and keep the higher alignment.) This is a regression
 * test for a bug in the reference implementation. Note that we can only catch
 * this bug once we implement constant folding; before then, we don't add
 * positive StaticConstants.
 * No 'target' function here because we're just looking for correctness,
 * not inspecting assembly.
 * */

double x = 5.0;

int main(void) {
    double d = -0.0;  // add normal constant -0. to list of top-level constants
    return (-x > d); // add 16-byte-aligned constant -0. to negate x
}
