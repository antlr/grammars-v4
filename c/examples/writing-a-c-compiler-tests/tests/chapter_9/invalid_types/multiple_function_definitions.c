/* Function 'foo' is defined twice */
int foo(void){
    return 3;
}

int main(void) {
    return foo();
}

int foo(void){
    return 4;
}