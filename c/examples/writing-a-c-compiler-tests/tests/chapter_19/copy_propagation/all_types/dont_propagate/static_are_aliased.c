/* Test that we consider all static variables aliased,
 * so store kills copies to/from these variables */
int stat;

int target(int *stat_ptr) {
    int a = 0;
    a = stat;       // gen a = stat
    *stat_ptr = 8;  // kill a = stat
    return a;       // make sure we don't rewrite as 'return stat'
}

int main(void) {
    int *ptr = &stat;
    stat = 5;
    int result = target(ptr);
    if (result != 5) {
        return 1;  // fail
    }
    if (stat != 8) {
        return 2;  // fail
    }
    return 0;  // success
}