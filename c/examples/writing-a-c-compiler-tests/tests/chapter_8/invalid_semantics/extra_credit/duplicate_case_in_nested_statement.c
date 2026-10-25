// Can't have duplicate cases in same enclosing switch, even in different scopes
int main(void) {
    int a = 10;
    switch (a) {
        case 1: {
            if(1) {
                case 1: // duplicate of previous 'case 1'
                return 0;
            }
        }
    }
    return 0;
}