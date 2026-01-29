/* Test that we delete ++/-- with dead variables */

#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif

static int glob;

int target(void) {
    // initialize these so they can't be constant-folded
    int a = glob;
    int b = glob;
    int c = glob;
    int d = glob;
    // these operations are all dead stores so we'll eliminate them
    a++;
    b--;
    ++c;
    --d;
    return 10;
}

int main(void) {
    return target();
}