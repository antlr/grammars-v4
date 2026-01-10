#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-label"
#endif
int main(void) {
    goto labelB;

    labelA:
        labelB:
            return 5;
    return 0;
}