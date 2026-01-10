/* Function 'foo' is defined twice */
int foo(void){
    return 3;
}

int main(void) {
    // after seeing this declaration, we should still remember that
    // foo was defined earlier
    int foo(void);
    return foo();
}

int foo(void){
    return 4;
}