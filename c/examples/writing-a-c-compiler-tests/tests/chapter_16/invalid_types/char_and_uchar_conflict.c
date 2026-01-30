/* char and unsigned char are different types; you can't use them
 * interchangeably when redeclaring the same identifier */
int foo(unsigned char c) {
    return c;
}

int main(void) {
    return foo(0);
}

// This redeclares foo with a different type than before
int foo(char c);