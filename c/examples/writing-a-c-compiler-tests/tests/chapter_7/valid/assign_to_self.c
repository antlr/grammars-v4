#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
int main(void) {
    int a = 3;
    {
        int a = a = 4;
        return a;
    }
}