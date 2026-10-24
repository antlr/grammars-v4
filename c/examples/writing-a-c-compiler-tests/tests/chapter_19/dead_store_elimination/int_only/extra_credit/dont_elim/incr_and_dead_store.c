#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

static int x = 10;

int main(void) {
    // incrementing x is not a dead store, although assigning to y is
    int y = ++x;
    return x;
}