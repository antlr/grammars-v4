// Can't perform bitwise operations with void operands
int main(void) {
    int x = 10;
    int y = 11;
    x & (void) y;
    return 0;
}