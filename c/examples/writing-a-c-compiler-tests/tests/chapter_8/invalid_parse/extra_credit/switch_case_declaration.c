int main(void) {
    switch(3) {
        case 3:
            // This is invalid in C17, but valid in C23;
            // we're targeting C17 so we reject it.
            // Run GCC or Clang with --std=c17 --pedantic to see diagnostics
            // More details: https://github.com/nlsandler/writing-a-c-compiler-tests/issues/104
            int i = 0;
            return i;
    }
    return 0;
}