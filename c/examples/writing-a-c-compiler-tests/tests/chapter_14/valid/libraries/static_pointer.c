// define a pointer that can be read/written only through functions
// note that these functions read/write the pointer itself, not the object it points to

static long *long_ptr;

long *get_pointer(void) {
    return long_ptr;
}

int set_pointer(long *new_ptr) {
    long_ptr = new_ptr;
    return 0;
}