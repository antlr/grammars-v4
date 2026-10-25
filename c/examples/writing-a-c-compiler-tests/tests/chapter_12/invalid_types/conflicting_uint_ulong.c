/* Can't declare the same function with two return types: unsignd int and unsigned long */
unsigned int foo(void);

unsigned long foo(void) {
    return 0;
}

int main(void) {
    return 0;
}