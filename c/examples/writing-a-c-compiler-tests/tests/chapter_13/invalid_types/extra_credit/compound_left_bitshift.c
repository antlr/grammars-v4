int main(void) {
    // Can't perform compound bitwise operations with doubles
    double d = 1.0;
    d <<= 1;
    return d;
}