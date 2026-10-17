int main(void) {
    // Can't perform compound bitwise operations with doubles
    double d = 1.0;
    d &= 0;
    return (int) d;
}