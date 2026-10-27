// It's illegal to dereference a label
int main(void) {
    lbl:
    *lbl;
    return 0;
}