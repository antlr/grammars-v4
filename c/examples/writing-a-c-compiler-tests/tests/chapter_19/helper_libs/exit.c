/* This just defines a wrapper for the exit system call
 * that we can use if we've only completed part I and so don't
 * support the 'void' return type.
 * Used in unreachable_code_elimination/infinite_loop.c
 * */

void exit(int status); // from standard library

int exit_wrapper(int status) {
    exit(status);
    return 0; // never reached
}