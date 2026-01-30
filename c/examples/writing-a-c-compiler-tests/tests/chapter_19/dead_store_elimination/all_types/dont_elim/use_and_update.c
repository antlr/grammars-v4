/* Test that updating and using a value in the same AddPtr instruction
 * generates it rather than killing it.
 * Most instructions won't use and update the same variable,
 * since their destinations are temporary variables that are used updated
 * only once, but implement &x.member  as:
 *   tmp = &x
 *   tmp = AddPtr(tmp, <offset of member>, scale=1)
 * So we can validate that AddPtr generates tmp rather than killing it.
 * */

struct s {
    int a;
    int b;
    int c;
};

struct s global_struct = {1, 2, 3};

int *target(void) {
    return &global_struct.b;
}

int main(void) {
    return *target() == 2;
}