// make sure we don't treat different cases as different scopes

int main(void) {
    int a = 1;
    switch (a) {
        case 1:;
            int b = 10;
            break;

        case 2:;
            // invalid redefinition, because we're in the same scope
            // as declaration of b above
            int b = 11;
            break;

        default:
            break;
    }
    return 0;
}