/* It's illegal to compare two different pointer types */
int main(void) {
    int *x = 0ul;
    unsigned *y = 0ul;
    return x == y;
}