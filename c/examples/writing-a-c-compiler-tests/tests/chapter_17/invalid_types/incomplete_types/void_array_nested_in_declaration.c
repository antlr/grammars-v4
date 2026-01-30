extern void (*ptr)[3][4];  // array of incomplete element type is illegal (including nested array)

void *foo(void) {
    return ptr;
}