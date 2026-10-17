// It's illegal to take the address of a label
int main(void) {
    int x = 0;
    lbl:
    x = 1;
    if (&lbl == 0) {
        return 1;
    }
    goto lbl;
    return 0;

}