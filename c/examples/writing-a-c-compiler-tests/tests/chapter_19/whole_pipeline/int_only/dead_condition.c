/* If a variable is only used as the controlling condition for an empty branch
 * we can eliminate the branch, then eliminate any updates to that variable,
 * because they'll all be dead stores
 * */
#if defined SUPPRESS_WARNINGS && !defined __clang__
#pragma GCC diagnostic ignored "-Wempty-body"
#endif

// flag is a global variable, not parameters
// so we don't have any instructions setting up function parameters,
// e.g. movl %edi, -4(%rbp), which the test script will complain about
int flag = 1;

int target(void) {
    int x = 2;
    if (flag) {
        x = 20;  // this will be a dead store after we remove branch below
                 // wrap this in an if statement so we don't propagate 20 into
                 // controlling condition
    }

    if (x)
        ;

    // we can eliminate the whole function body except this return statement
    return 10;
}

int main(void) {
    return target();
}