/* Test that we can coalesce one-byte operands. Validate that there are no
 * spills and no mov instructions between registers.
 * */

#include "../util.h"

char glob_a;
char glob_b;
char glob_c;
char glob_d;
char glob_e;
char glob_f;

int glob = 0;
void set_glob(void) {
    glob = 10;
}

// Make sure we coalesce a-f into param-passing registers
// We save them to global variables and validate those, instead of
// validating a-f directly, because just calling check_six_chars on
// a-f would create interferences that would force us to put each
// pseudo in the corresponding register even if we didn't actually coalesce
// them.
int target(char a, char b, char c, char d, char e, char f) {
    // save to global variables
    glob_a = a;
    glob_b = b;
    glob_c = c;
    glob_d = d;
    glob_e = e;
    glob_f = f;
    set_glob();  // to prevent copy-prop
    // validate global variables
    check_six_chars(glob_a, glob_b, glob_c, glob_d, glob_e, glob_f, 1);
    check_one_int(glob, 10); // make sure we actually called set_glob
    return 0;
}