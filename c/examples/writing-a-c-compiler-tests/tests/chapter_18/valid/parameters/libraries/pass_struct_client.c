/* Basic test of passing an argument of structure type: similar to chapter_18/valid/parameters/simple.c
 * but split into two translation units
 * */

#include "pass_struct.h"

int main(void) {
    struct pair arg = {1, 2};
    if (!validate_struct_param(arg)) {
        return 1;
    }
    return 0; // success
}