// can't use string literal as controlling expression in switch statement
int main(void) {
    switch ("foo") {
        default:
        return 0;
    }
}