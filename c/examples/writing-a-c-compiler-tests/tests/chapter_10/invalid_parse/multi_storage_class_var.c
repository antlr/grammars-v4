int main(void) {
    /* a variable can't have more than one storage class */
    static extern int foo = 0;
    return foo;
}