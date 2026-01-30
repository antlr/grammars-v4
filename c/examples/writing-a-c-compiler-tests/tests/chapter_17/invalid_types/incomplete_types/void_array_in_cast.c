int main(void) {
    (void(*)[3]) 4; // the element type in an array declarator must be complete
    return 0;
}