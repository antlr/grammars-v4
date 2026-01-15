// It's illegal to use pointer as right operand in bitshift
int main(void) {
    int *ptr = 0;
    int i = 1000;
    i >> ptr;
    return 0;
}