int foo(int a, int b);

int main(void) {
    return foo(2, 1);
}

/* Multiple declarations of a function
 * can use different parameter names
 */
int foo(int x, int y){
    return x - y;
}