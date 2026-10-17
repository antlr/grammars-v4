#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
int main(void) {
    int a = 2;
    {
        int a = 1;
        return a;
    }
}