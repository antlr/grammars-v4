int main(void) {
    int x = 1;
    goto post_declaration;
    // we skip over initializer, so it's not executed
    int i = (x = 0);
post_declaration:
    // even though we didn't initialize i, it's in scope, so we can use it
    i = 5;
    return (x == 1 && i == 5);
}