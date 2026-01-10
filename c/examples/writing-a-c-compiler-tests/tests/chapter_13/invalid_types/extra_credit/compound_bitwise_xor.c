int main(void) {
    // Can't perform compound bitwise operations with doubles
    int i = 0;
    i |= 2.0;
    return (int) i;
}