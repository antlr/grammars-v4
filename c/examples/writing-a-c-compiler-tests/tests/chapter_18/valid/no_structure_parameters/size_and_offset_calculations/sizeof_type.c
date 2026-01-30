/* Verify that sizeof produces correct results for various structure types (and
 * arrays of structs) */

#include "struct_sizes.h"

int main(void) {
    // validate the size of every type in struct_sizes.h

    if (sizeof(struct eight_bytes) != 8) {
        return 1;
    }

    if (sizeof(struct two_bytes) != 2) {
        return 2;
    }

    if (sizeof(struct three_bytes) != 3) {
        return 3;
    }

    if (sizeof(struct sixteen_bytes) != 16) {
        return 4;
    }

    if (sizeof(struct seven_bytes) != 7) {
        return 5;
    }

    if (sizeof(struct twentyfour_bytes) != 24) {
        return 6;
    }

    if (sizeof(struct twenty_bytes) != 20) {
        return 7;
    }

    if (sizeof(struct wonky) != 19) {
        return 8;
    }

    if (sizeof(struct internal_padding) != 16) {
        return 9;
    }

    if (sizeof(struct contains_struct_array) != 28) {
        return 10;
    }

    if (sizeof(struct internal_padding[4]) != 64) {
        return 11;
    }

    if (sizeof(struct wonky[2]) != 38) {
        return 12;
    }

    return 0;  // success
}