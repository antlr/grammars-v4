int main(void) {
    // can't declare two structures with the same tag
    // in the same scope
    struct x {
        int x;
    };
    struct x {
        int y;
    };
    return 0;
}