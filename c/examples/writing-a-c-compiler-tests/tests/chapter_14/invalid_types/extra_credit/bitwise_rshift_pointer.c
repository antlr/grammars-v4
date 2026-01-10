/* It's illegal to apply left or right bitshift operations to pointers */
int main(void) {
    int *x = 0;
    return (int) (x >> 10);
}