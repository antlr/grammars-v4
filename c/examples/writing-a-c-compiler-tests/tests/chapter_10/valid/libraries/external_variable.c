/* this function defines a variable with external linkage */

/* a tentative definition of x;
 * this will be treated like a declaration,
 * since x is defined later */
int x;

/* a variable with external linkage can be tentatively defined
 * and declared multiple times in a file
 */
extern int x;
int x;

int update_x(int new_val) {
    x = new_val;
    return 0;
}

int read_x(void) {
    return x;
}

/* the definition of x */
int x = 3;