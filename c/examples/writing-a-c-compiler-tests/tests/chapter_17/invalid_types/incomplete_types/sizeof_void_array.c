int main(void) {
    // the element type in an array declarator must be complete
    return sizeof(void[3]);
}