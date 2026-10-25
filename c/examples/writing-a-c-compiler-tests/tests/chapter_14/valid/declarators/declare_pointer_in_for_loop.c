int main(void) {
    int x = 10;
    // make sure we can handle pointer declarators
    // in for loop initializers
    for (int *i = &x; i != 0; ) {
        *i = 5;
        i = 0;
    }
    return x;
}