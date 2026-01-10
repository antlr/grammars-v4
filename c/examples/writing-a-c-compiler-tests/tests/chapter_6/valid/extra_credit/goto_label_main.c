#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-label"
#endif
int main(void) {
    // it's legal to use main as both a function name and label
    goto main;
    return 5;
main:
    return 0;
}