#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wswitch-unreachable"
#endif
#endif

// if a switch statement body contains no case statements,
// nothing in it will be executed
int main(void) {
    int a = 4;
    switch(a)
        return 0;
    return a;
}