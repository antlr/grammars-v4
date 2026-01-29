/* Copy to one member of a union kills previous copy to other member.
 * The easiest way to handle this, which is in line with how we handle structs,
 * is to not attempt to propagate copies to/from union members at all.
 * But if you do implement a more sophisticated copy propagation pass that
 * tracks copies to/from union members, you need to account for the fact
 * that they overlap.
 */

union u {
    long l;
    int i;
};

int main(void) {
    union u x;
    x.i = 100;
    x.l = 200; // clobber x.i;
    return x.i; // should be 200 (due to type punning), not 100
}