int foo(void) {
    label:
        return 0;
}

int main(void) {
    /* You can't goto a label in another function */
    goto label;
    return 1;
}