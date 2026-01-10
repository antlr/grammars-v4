unsigned int ui = 4294967200u;

unsigned int return_uint(void) {
    return ui;
}

int return_uint_as_signed(void) {
    return ui; //implicitly convert to signed int
}

long return_uint_as_long(void) {
    return ui; // implicitly convert to signed long
}