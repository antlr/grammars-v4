int main(void) {
    // make sure our validation of switch statements also traverses labeled
    // statements
    int a = 0;
label:
    switch (a) {
        case 1:
        case 1:
            break;
    }
    return 0;
}