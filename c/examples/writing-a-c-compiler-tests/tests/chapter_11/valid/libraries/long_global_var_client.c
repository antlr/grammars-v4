/* Make sure we can read and write long integers in other translation units */

// the following are defined in long_global_var.c
extern long int l;
long return_l(void);
int return_l_as_int(void);



int main(void) {

    /* Make sure l has the right value before we update it */
    if (return_l() != 8589934592l)
        return 1;

    if (return_l_as_int() != 0)
        return 2;

    /* Update l */
    l = l - 10l;

    /* Read back the value we just assigned to l */
    if (return_l() != 8589934582l)
        return 3;

    if (return_l_as_int() != -10)
        return 4;

    return 0;
}