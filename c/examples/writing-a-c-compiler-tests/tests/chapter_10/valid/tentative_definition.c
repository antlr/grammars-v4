/* Verify that if variable is tentatively defined one or more times,
 * but not explicitly initialized, we'll initialize it to 0.
 */

/* This declares foo but does not define it */
extern int foo;

/* A tentative definition of foo */
int foo;

/* Another tentative definition of foo */
int foo;

int main(void) {
    for (int i = 0; i < 5; i = i + 1)
        foo = foo + 1;
    return foo;
}

/* Yet another tentative definition of foo */
int foo;