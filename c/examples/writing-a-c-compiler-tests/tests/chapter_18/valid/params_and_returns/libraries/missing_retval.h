/* Test case where the return type would be passed on the stack, but the callee
 * is missing a return statement This is well-defined as long as the caller
 * doesn't try to use the return value
 * */
struct big {
    char arr[25];
};

struct big missing_return_value(int *i);