/* Test that we correctly handle pointers to pointers */
int main(void) {

    // set up chain of indirection:
    // d_ptr_ptr_ptr > d_ptr_ptr > d_ptr > d = 10.0

    double d = 10.0;
    double *d_ptr = &d;
    double **d_ptr_ptr = &d_ptr;
    double ***d_ptr_ptr_ptr = &d_ptr_ptr;

    // read value of d through multiple levels of indirection
    if (d != 10.0) {
        return 1;
    }
    if (*d_ptr != 10.0) {
        return 2;
    }

    if (**d_ptr_ptr != 10.0) {
        return 3;
    }

    if (***d_ptr_ptr_ptr != 10.0) {
        return 4;
    }

    // read address of d through multiple levels of indirection
    if (&d != d_ptr) {
        return 5;
    }
    if (*d_ptr_ptr != d_ptr) {
        return 6;
    }
    if (**d_ptr_ptr_ptr != d_ptr) {
        return 7;
    }

    // update value of d through multiple levels of indirection
    // now d_ptr_ptr_ptr > d_ptr_ptr > d_ptr > d = 50
    ***d_ptr_ptr_ptr = 5.0;
    if (d != 5.0) {
        return 8;
    }
    if (*d_ptr != 5.0) {
        return 9;
    }
    if (**d_ptr_ptr != 5.0) {
        return 10;
    }

    if (***d_ptr_ptr_ptr != 5.0) {
        return 11;
    }

    // new chain of indirection:
    // d_ptr_ptr_ptr > d2_ptr_ptr > d2_ptr > d2 = 1.0
    //                              d2_ptr2 -^
    double d2 = 1.0;

    // make both d2_ptr and d2_ptr2 point to same variable, d
    double *d2_ptr = &d2;
    double *d2_ptr2 = d2_ptr;

    // define a pointer to d2_ptr
    double **d2_ptr_ptr = &d2_ptr;

    // make d_ptr_ptr_ptr point to chain that leads back to d2, not d
    *d_ptr_ptr_ptr = d2_ptr_ptr;

    // validate dereference through d_ptr_ptr_ptr now that we've updated it
    if (**d_ptr_ptr_ptr != d2_ptr) {
        return 12;
    }

    if (***d_ptr_ptr_ptr != 1.0) {
        return 13;
    }

    // even though d2_ptr and d2_ptr2 have the same value,
    // they don't have the same address;
    // d2_ptr_ptr points to d2_ptr but not d2_ptrs
    if (d2_ptr_ptr == &d2_ptr2)
        return 14;

    // changing the value of d2_ptr also changes *d2_ptr_ptr and **d_ptr_ptr_ptr,
    // but d2_ptr2 is unchanged
    //                  d_ptr_ptr > d_ptr ---v
    // d_ptr_ptr_ptr > d2_ptr_ptr > d2_ptr > d1 = 5.0
    //                              d2_ptr2 -> d2 = 1.0
    d2_ptr = d_ptr;

    if (**d_ptr_ptr_ptr != d_ptr) {
        return 15;
    }


    if (*d2_ptr_ptr != d_ptr) {
        return 16;
    }

    // *d_ptr_ptr_ptr now points to d1, d2_ptr2 points to d2
    if (**d_ptr_ptr_ptr == d2_ptr2) {
        return 17;
    }

    if (***d_ptr_ptr_ptr != 5.0) {
        return 18;
    }

    return 0;
}