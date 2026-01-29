/* Test that we can pass a pointer to an array of structures as a parameter */

#include "array_of_structs.h"

int validate_struct_array(struct outer *struct_array) {
    for (int i = 0; i < 3; i = i + 1) {
        if (struct_array[i].a != i * 2)
            return 0;
        if (struct_array[i].b.l != i * 3)
            return 0;
        if (struct_array[i].b.arr[0] != i * 4)
            return 0;
        if (struct_array[i].b.arr[1] != i * 5)
            return 0;
    }
    return 1;  // success
}