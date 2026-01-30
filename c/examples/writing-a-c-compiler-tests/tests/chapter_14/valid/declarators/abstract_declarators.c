/* Test out parsing a range of abstract declarators */
int main(void) {

    long int unsigned *x = 0;

    /* Use a couple of equivalent abstract declarators
     * to cast 0 to a pointer to unsigned long
     */
    if (x != (unsigned long (*)) 0)
        return 1;

    if (x != (long unsigned int ((((*))))) 0)
        return 2;

    double ***y = 0;

    /* Use several equivalent abstract declarators
     * to cast 0 to (double ***)
     */
    if (y != (double *(**)) 0)
        return 3;

    if (y != (double (***)) 0)
        return 4;

    if ((double (*(*(*)))) 0 != y)
        return 5;

    return 0;
}