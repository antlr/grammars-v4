int main(void) {
    int a = 0;
    switch(a) {
        case 0: return 0;
        default: return 1;
        case 2: return 2;
        // can't have two default statements in same enclosing switch
        default: return 2;
    }
}