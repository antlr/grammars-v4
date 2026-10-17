#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wsign-compare"
#endif

// Test out bitwise operations on dereferenced pointers,
// including ones that require implicit type conversions.
// This performs the same calculations as tests/chapter_12/valid/extra_credit/bitwise_unsigned_ops.c
// but through dereferenced pointers
int main(void) {
    unsigned int ui = -1u; // lower 32 bits set
    unsigned long ul = 9223372036854775808ul; // 2^63, only uppermost bit set
    unsigned int *ui_ptr = &ui;
    unsigned long *ul_ptr = &ul;

    if ((*ui_ptr & *ul_ptr) != 0) {
        return 1;
    }

    if ((*ui_ptr | *ul_ptr) != 9223372041149743103ul) {
        return 2;
    }

    int i = -1;
    signed int *i_ptr = &i;
    if ((*i_ptr & ul) != *ul_ptr) {
        return 3;
    }

    if ((*i_ptr | *ul_ptr) != i) {
        return 4;
    }

    return 0; // success
}