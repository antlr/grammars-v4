#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wdangling-else"
#endif
int main(void) {
    int a = 0;
    if ( (a = 1) )
        if (a == 1)
            a = 3;
        else
            a = 4;

    return a;
}