/* Test passing pointers to allocated memory between translation units */


void *get_100_zeroed_bytes(void);
void fill_100_bytes(void *pointer, int byte);
void free_bytes(void *ptr);

int main(void) {

    void *mem = get_100_zeroed_bytes();
    // make sure it's all zeroed
    for (int i = 0; i < 100; i = i + 1) {
        if (((char *) mem + i)[0]) {
            return 1;
        }
    }

    // populate it
    fill_100_bytes(mem, 99);

    // make sure every byte is set to 99
    for (int i = 0; i < 100; i = i + 1) {
        if (((char *) mem + i)[0] != 99) {
            return 2;
        }
    }

    // free it
    free_bytes(mem);

    return 0;
}