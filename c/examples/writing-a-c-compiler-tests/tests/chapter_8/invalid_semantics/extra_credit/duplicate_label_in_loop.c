int main(void) {
    do {
        // make sure our label-validation analysis also traverses loop bodies
    lbl:
        return 1;
    lbl:
        return 2;
    } while (1);
    return 0;
}