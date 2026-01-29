// Test out bitshift operations on dereferenced pointers
// Same calculations as in tests/chapter_12/valid/extra_credit/bitwise_unsigned_shift.c
// but through pointers

unsigned int ui = 4294967295; // 2^32 - 1

unsigned int *get_ui_ptr(void){
    return &ui;
}

int shiftcount = 5;

int main(void) {

    // use dereferenced pointer as left operand
    if ((*get_ui_ptr() << 2l) != 4294967292) {
        return 1;
    }

    if ((*get_ui_ptr() >> 2) != 1073741823) {
        return 2;
    }

    // also use dereferenced pointer as right operand
    int *shiftcount_ptr = &shiftcount;
    if ((1000000u >> *shiftcount_ptr) != 31250) {
        return 3;
    }
    if ((1000000u << *shiftcount_ptr) != 32000000) {
        return 4;
    }

    return 0;  // success
}