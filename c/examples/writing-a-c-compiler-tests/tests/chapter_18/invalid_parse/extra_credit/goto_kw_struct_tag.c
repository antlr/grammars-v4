// Because 'goto' is a keyword, we can't use it as a struct tag
struct goto { int a; };
int main(void) {
    return 0;
}