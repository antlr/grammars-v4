int main(void) {
    int a = 1;
    // a switch contains cases but none match, and there's no default;
    // no part of the body should be executed
    switch(a) {
        case 0: return 0;
        case 2: return 0;
        case 3: return 0;
    }
    return 1;
}