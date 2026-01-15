/* This file defines and uses a variable with internal linkage. */

/* a tentative definition of x - variables with internal linkage may be tentatively
 * defined and declared multiple times.
 */
static int x;

int read_x(void) {
    return x;
}

int update_x(int new_val) {
    /* this declaration refers to the 'x'
     * identifier that is already in scope,
     * and takes on the same linkage
     */
    extern int x;
    x = new_val;
    return 0;
}


/* since a declaration of x with internal linkage is already in scope,
 * this takes on that linkage.
 */
extern int x;

// a non-tentative definition of x
static int x = 5;

// yet another tentative definition of x
static int x;
