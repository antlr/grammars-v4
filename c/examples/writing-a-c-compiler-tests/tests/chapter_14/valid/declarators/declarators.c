/* Test that we can parse a variety of function and variable declarators */

/* Multiple equivalent declarations of the function 'return_3' */
int return_3(void);
int(return_3(void));
int(return_3)(void);
int((return_3))(void)
{
    return 3;
}


long l = 100; // used below
/* Multiple equivalent declarations of the function 'two_pointers' */
long *two_pointers(double val, double *ptr)
{
    *ptr = val;
    return &l;
}
long(*two_pointers(double val, double(*d)));
long *(two_pointers)(double val, double *(d));
long *(two_pointers)(double val, double(*(d)));

/* Multiple equivalent declarations of the function 'pointers_to_pointers' */
unsigned **pointers_to_pointers(int **p)
{
    static unsigned u;
    static unsigned *u_ptr;
    u_ptr = &u;
    u = **p;
    return &u_ptr;
}
unsigned(**(pointers_to_pointers(int *(*p))));
unsigned *(*pointers_to_pointers(int(**p)));
unsigned(*(*((pointers_to_pointers)(int(*(*(p)))))));

int main(void)
{
    /* Declare some variables using a variety of declarators */
    int i = 0;
    int(*i_ptr) = &i;
    int(**ptr_to_iptr) = &i_ptr;

    double(d1) = 0.0;
    double d2 = 10.0;

    double *(d_ptr) = &d1;

    long(*(l_ptr));

    unsigned *(*(ptr_to_uptr));

    /* Use functions and variables we just declared */
    i = return_3(); // assign 3 to i
    if (i != 3) // this also updates ptr_to_iptr
        return 1;

    if (*i_ptr != 3) {
        return 2;
    }

    // call two_pointers and validate the results
    l_ptr = two_pointers(d2, d_ptr);
    // l_ptr is a pointer to static variable l declared above
    if (l_ptr != &l) {
        return 3;
    }

    if (*l_ptr != 100) {
        return 4;
    }

    // two_pointers also assigned value of d2 (10.0) to
    // object referenced by d_ptr, which is d1
    if (*d_ptr != 10.0) {
        return 5;
    }

    if (d1 != 10.0) {
        return 6;
    }


    // call pointers_to_pointers and validate the results
    ptr_to_uptr = pointers_to_pointers(ptr_to_iptr);

    if (**ptr_to_uptr != 3) {
        return 7;
    }

    return 0;
}