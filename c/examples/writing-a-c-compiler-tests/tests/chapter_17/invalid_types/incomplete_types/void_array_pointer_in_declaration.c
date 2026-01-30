void *malloc(unsigned long size);

int main(void) {
    void (*ptr)[3] = malloc(3); // array of incomplete element type is illegal
    return ptr == 0;
}