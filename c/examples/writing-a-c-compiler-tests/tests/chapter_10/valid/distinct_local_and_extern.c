/* verify that automatic and external variables with the same name
 * are distinct, can be read and updated separately,
 * and can shadow one another
 */

/* a global variable 'a' */
int a = 5;

int return_a(void) {
    /* return the current value of the global variable */
    return a;
}

int main(void) {
    /* automatic variable 'a', distinct from the global variable 'a' */
    int a = 3;
    {
        /* this declaration refers to the global variable,
         * shadowing the automatic variable declared above
         */
        extern int a;
        if (a != 5)
            return 1;
        /* update global variable */
        a = 4;
    }
    /* return the sum of the global and local 'a' variables */
    return a + return_a();
}