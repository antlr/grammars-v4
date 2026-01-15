#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#pragma GCC diagnostic ignored "-Wswitch-unreachable"
#endif
#endif
// a switch statement may jump into the middle of other control flow constructs

int main(void) {
    int switch1 = 0;
    int switch2 = 0;
    int switch3 = 0;
    switch(3) {
        case 0: return 0;
        case 1: if (0) {
            case 3: switch1 = 1; break;
        }
        default: return 0;
    }
    switch(4) {
        case 0: return 0;
        if (1) {
            return 0;
        } else {
            case 4: switch2 = 1; break;
        }
        default: return 0;
    }
    switch (5) {
        for (int i = 0; i < 10; i = i + 1) {
            switch1 = 0;
            case 5: switch3 = 1; break;
            default: return 0;
        }
    }

    return (switch1 && switch2 && switch3);
}