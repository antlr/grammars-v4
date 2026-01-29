// an external variable is in scope inside a switch statement
// even if we jump over the point where it's declared

int main(void) {
    int a = 10;
    switch(a) {
        case 1: return 1; // fail
        extern int x; // bring x into scope
        case 2: return 2; // fail
        case 10:
        if (x * 2 == 30) {
            return 0; // success
        }
        default: return 5; // fail
    }
    return 6; // also fail; shouldn't have made it to this point
}


int x = 15;