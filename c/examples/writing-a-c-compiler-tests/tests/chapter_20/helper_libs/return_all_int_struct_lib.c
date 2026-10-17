#include <stdio.h>
#include <stdlib.h>

struct s {
    int a;
    int b;
    long l;
};

// defined in tests/chapter_20/all_types/no_coalescing/return_all_int_struct.c
struct s return_struct(void);

int target(void) {
    struct s retval = return_struct();
    if (retval.a != 20) {
        printf("Expected retval.a to have value 20, actual value was %d\n",
               retval.a);
        exit(-1);
    }
    if (retval.b != 30) {
        printf("Expected retval.b to have value 30, actual value was %d\n",
               retval.b);
        exit(-1);
    }
    if (retval.l != 40) {
        printf("Expected retval.l to have value 40, actual value was %ld\n",
               retval.l);
        exit(-1);
    }
    return 0;
}