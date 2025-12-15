/* Test passing struct pointers as function parameters */

#include "param_struct_pointer.h"

int main(void) {
    struct outer s = {1, 2, {3.0, 4}};
    if (!access_members_through_pointer(&s, 1, 2, 3.0, 4)) {
        return 1;
    }

    struct inner inner_struct = {7, 8};
    update_members_through_pointer(&s, 5, 6, &inner_struct);
    if (s.a != 5 || s.b != 6 || s.substruct.d != 7 || s.substruct.i != 8) {
        return 2;
    }

    return 0;  // success
}