int main(void) {
    // take the bitwise complement of the smallest int we can construct right now
    // (minimum representable int is actually -2147483648, but we can't
    // construct it b/c the constant 2147483648 is out of bounds)
    return ~-2147483647;
}