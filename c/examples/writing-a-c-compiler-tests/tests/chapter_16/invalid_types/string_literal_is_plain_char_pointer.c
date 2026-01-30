/* String literals decay to char * arrays,
 * and can't initialize pointers to other types. */
int main(void) {
    // ptr is a signed char *, but "foo" decays to type char *
    signed char *ptr = "foo";
    return 0;
}