int main(void) {
    /* An extern variable cannot have an initializer */
    extern int i = 0;
    return i;
}