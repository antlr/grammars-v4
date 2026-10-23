/* A variable with internal linkage may be tentatively defined
 * and declared multiple times, but defined only once
 */

/* A tentative definition */
static int foo;

int main(void) {
    return foo;
}

/* A declaration */
extern int foo;

/* A non-tentative definition */
static int foo = 4;