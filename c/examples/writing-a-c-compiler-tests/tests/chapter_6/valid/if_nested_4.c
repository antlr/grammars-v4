#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wdangling-else"
#endif
int main(void) {
    int a = 0;
    if (!a)
        if (3 / 4)
            a = 3;
        else
            a = 8 / 2;

    return a;
}