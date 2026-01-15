/* It's illegal to cast a double to a pointer */

int main(void) {
    double d = 0.0;
    int *x = (int *) d;
    return 0;
}