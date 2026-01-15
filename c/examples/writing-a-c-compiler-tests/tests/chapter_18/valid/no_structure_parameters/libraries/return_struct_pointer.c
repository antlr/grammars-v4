/* Test returning struct pointers from functions
 * and using struct pointers returned from functions
 * */

#include "return_struct_pointer.h"

// define some functions that return pointers to structs
struct inner *make_struct_inner(int seed) {
    struct inner *ptr = malloc(sizeof(struct inner));
    ptr->d = seed;
    ptr->i = seed;
    return ptr;
}

struct outer *make_struct_outer(int seed) {
    struct outer *ptr = malloc(sizeof(struct outer));
    ptr->a = seed;
    ptr->b = seed + 1;
    ptr->substruct.d = seed + 2;
    ptr->substruct.i = seed + 3;
    return ptr;
}

struct outermost *make_struct_outermost(int seed) {
    struct outermost *ptr = malloc(sizeof(struct outermost));
    ptr->i = seed;
    ptr->nested_ptr = make_struct_outer(seed + 1);
    ptr->nested_struct.a = seed + 5;
    ptr->nested_struct.b = seed + 6;
    ptr->nested_struct.substruct.d = seed + 7;
    ptr->nested_struct.substruct.i = seed + 8;
    return ptr;
}