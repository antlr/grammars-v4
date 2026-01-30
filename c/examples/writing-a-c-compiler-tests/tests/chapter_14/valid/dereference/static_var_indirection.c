/* Test pointers to static objects, and static pointers to automatic objects */

unsigned int w = 4294967295U;
int x = 10;
unsigned int y = 4294967295U;
double *dbl_ptr;

long modify_ptr(long *new_ptr) {
    static long *p;
    if (new_ptr)
    {
        p = new_ptr;
    }
    return *p;
}


int increment_ptr(void)
{
    *dbl_ptr = *dbl_ptr + 5.0;
    return 0;
}

int main(void) {
    // get a pointer to a static variable

    int *pointer_to_static = &x;
    x = 20;
    // make sure we can read new value through pointer
    if (*pointer_to_static != 20) {
        return 1;
    }

    // now update value through pointer
    *pointer_to_static = 100;

    // make sure x and neighboring variables have correct values

    if (x != 100) {
        return 2;
    }
    if (w != 4294967295U) {
        return 3;
    }
    if (y != 4294967295U) {
        return 4;
    }
    if (dbl_ptr) {
        return 5;
    }

    // now try updating a pointer that is itself static
    long l = 1000l;

    // make static pointer in modify_ptr point to l
    if (modify_ptr(&l) != 1000l) {
        return 6;
    }

    // update l, make sure sure p in modify_ptr reflects that
    l = -1;
    // get value of p - pass null pointer as argument so p doesn't change
    if (modify_ptr(0) != l) {
        return 7;
    }

    // finally, try updating a variable through a global pointer
    double d = 10.0;
    dbl_ptr = &d;
    increment_ptr();
    if (*dbl_ptr != 15) {
        return 8;
    }

    return 0;
}