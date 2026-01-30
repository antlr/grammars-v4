// a switch statement cannot jump to cases in a nested switch statement
int main(void) {
    int a = 0;
    // outer switch will execute default, not nested 'case 0'
    switch(a) {
        case 1:
            switch(a) {
                case 0: return 0;
                default: return 0;
            }
        default: a = 2;
    }
    return a;
}