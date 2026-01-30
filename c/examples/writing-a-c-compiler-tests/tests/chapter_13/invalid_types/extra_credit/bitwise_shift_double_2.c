int main(void) {
    /* It's illegal to use a double as the right operand of << or >>. */
    return 1 << 2.0;
}