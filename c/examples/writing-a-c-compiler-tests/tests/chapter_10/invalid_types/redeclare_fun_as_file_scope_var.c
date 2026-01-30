int foo(void);

/* this conflict with the previous declaration of foo as a function */
int foo;

int main(void) {
    return 0;
}
