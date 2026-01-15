#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-label"
#endif
int main(void) {
    goto _main;
    return 0;
    _main:
        return 1;
}