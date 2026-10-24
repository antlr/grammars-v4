// Test out compound bitwise operations with dereferenced pointers, including
// ones that involve implicit type conversions
// Same operations as tests/chapter_12/valid/extra_credit/compound_bitwise.c

unsigned long ul = 18446460386757245432ul; // 0xfffe_fdfc_fbfa_f9f8

int main(void) {

    unsigned long *ul_ptr = &ul;
    *ul_ptr &= -1000;
    if (ul != 18446460386757244952ul /* 0xfffe_fdfc_fbfa_f818 */) {
        return 1; // fail
    }
    *ul_ptr |= 4294967040u;

    if (ul != 18446460386824683288ul /* 0xfffe_fdfc_ffff_ff18 */) {
        return 2; // fail
    }
    int i = 123456;
    unsigned int ui = 4042322160u; // 0xf0f0_f0f0
    long l = -252645136; // 0xffff_ffff_f0f0_f0f0
    unsigned int *ui_ptr = &ui;
    long *l_ptr = &l;
    if (*ui_ptr ^= *l_ptr) {
        return 3; // fail
    }
    if (ui) {
        return 4;
    }

    // check neighbors
    if (i != 123456) {
        return 5;
    }
    if (l != -252645136) {
        return 6;
    }

    return 0; // success
}