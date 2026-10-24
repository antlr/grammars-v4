/* Test that we can pass a pointer to an array of structures as a parameter */

#include "array_of_structs.h"

static struct outer static_array[3] = {
    {0, {0, {0, 0}}}, {2, {3, {4, 5}}}, {4, {6, {8, 10}}}};

int main(void) {
    struct outer auto_array[3] = {
        {0, {0, {0, 0}}}, {2, {3, {4, 5}}}, {4, {6, {8, 10}}}};

    // pass pointers to struct arrays with both static and automatic storage
    // both have same contents so we can validate them with the same function

    if (!validate_struct_array(static_array)) {
        return 1;
    }

    if (!validate_struct_array(auto_array)) {
        return 2;
    }

    return 0;  // success
}