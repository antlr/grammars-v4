/* String literals decay to char * arrays,
 * and can't initialize pointers to other types.
 * This applies to static pointers too. */
int main(void) {
    // ptr is a signed char *, but "foo" decays to type char *
    static signed char *ptr = "foo";
    return 0;
}