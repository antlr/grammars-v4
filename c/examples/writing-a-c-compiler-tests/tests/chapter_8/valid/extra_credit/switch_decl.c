#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wswitch-unreachable"
#endif
#endif

int main(void) {
    int a = 3;
    int b = 0;
    switch(a) {
        // a is in scope but we skip its initializer
        int a = (b = 5);
    case 3:
        a = 4;
        b = b + a;
    }

    // make sure case was executed but initializer (b = 5) was not
    return a == 3 && b == 4;
}