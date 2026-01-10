/* A variation on listing_19_5.c with types other than int */

// make flag a global variable rather than a parameter
// so we don't have any instructions setting up function parameters,
// e.g. movl %edi, -4(%rbp), which the test script will complain about
double flag = 12e5;

struct inner {
    double a;
    double b;
};

struct s {
    void *ptr;
    long arr[5];
    struct inner x;
    char c[4];
};

long target(void) {
    unsigned long x = 4;
    char z;
    struct s my_struct = {&z,
                          {
                              1l,
                              2l,
                          },
                          {3., 4.},
                          "abc"};
    if (4 - x) {
        x = my_struct.c[2];
        z = my_struct.arr[1];
        my_struct.x.a = z * 100.;
    }
    if (!flag) {
        z = 10 + *(int *)my_struct.ptr;
    }
    z = x + 5;
    return z;
}

int main(void) {
    return target();
}