/* Test that sizeof expression results in an unsigned long */

#if defined SUPPRESS_WARNINGS && !defined __clang__
#pragma GCC diagnostic ignored "-Wtype-limits"
#endif

int main(void) {

    // sizeof result is a ulong, so _its_ size is 8
    if (sizeof sizeof (char) != 8) {
        return 1;
    }

    // make sure sizeof result is unsigned
    // since the common type of ulong and int is ulong,
    // the result of subtraction here will be positive unsigned int
    // (and 0 in comparison will also be converted to 0u)
    if (sizeof 4 - sizeof 4 - 1 < 0) {
        return 2;
    }

    return 0;
}