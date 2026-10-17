#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

/* Make sure we store a variable in a register if it's aliased in the
 * original program, but the aliasing gets optimized away.
 */

// shouldn't need to place any pseudos in this function on the stack
int target(int arg) {
    int *optimized_away = &arg; // dead store; will get optimized away
    return arg + 10;
}
