int main(void) {
    // test that variable resolution traverse the controlling epxression
    // in switch statements
    switch(a) {
        case 1: return 0;
        case 2: return 1;
    }
    return 0;
}