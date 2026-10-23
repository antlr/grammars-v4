int main(void) {
    /* Can't combine signed and unsigned specifiers */
    int i = 0;
    return (signed unsigned) i;
}