/* Basic test that we can access a global struct in multiple translation units
 * */
#include "global_struct.h"

struct s global = {1, {2, 3}, 4.0};
struct outer global_outer = {5, {6, {7, 8}, 9.0}};

int main(void) {
    /*
     * update_struct executes:
     * global.arr[1] = global.arr[0]*2;
     * global.d = 5.0;
     * */
    update_struct();
    if (global.arr[1] != 4) {
        return 1;
    }
    if (global.d != 5.0) {
        return 2;
    }

    /*
     * update_outer_struct executes:
     * struct global inner = {0, {-1, -1}, 0}
     * global_outer.inner = inner
     */
    update_outer_struct();
    if (global_outer.c != 5) {
        return 3;
    }

    if (global_outer.inner.i || global_outer.inner.d) {
        return 4;
    }

    if (global_outer.inner.arr[0] != -1 || global_outer.inner.arr[1] != -1) {
        return 5;
    }

    return 0;  // success
}