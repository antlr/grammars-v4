/* Basic test of passing an argument of structure type: similar to chapter_18/valid/parameters/simple.c
 * but split into two translation units
 * */

#include "pass_struct.h"

int validate_struct_param(struct pair p) {
    if (p.x != 1 || p.y != 2) {
        return 0;
    }

    return 1; // success
}