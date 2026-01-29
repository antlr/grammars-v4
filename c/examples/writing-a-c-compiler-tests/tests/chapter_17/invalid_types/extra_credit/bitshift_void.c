// Can't perform bitshift operations with void operands
void f(void){
    return;
}

int main(void) {
    int x = 10;
    x << f();
    return 0;
}