/* It's illegal to cast a pointer to a double */

int main(void) {
    int *x;
    double d = (double) x;
    return 0;
}