int main(void) {
    int a = 0;
    int b = 0;
    // a label does not start a new block, so you can't use it
    // to delineate a multi-statement loop body
    do
    do_body:
        a = a + 1;
        b = b - 1;
    while (a < 10)
        ;
    return 0;
}