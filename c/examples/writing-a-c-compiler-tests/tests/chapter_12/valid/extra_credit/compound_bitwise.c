int main(void) {

    unsigned long ul = 18446460386757245432ul; // 0xfffe_fdfc_fbfa_f9f8
    ul &= -1000; // make sure we sign-extend -1000 to unsigned long
    if (ul != 18446460386757244952ul /* 0xfffe_fdfc_fbfa_f818 */) {
        return 1; // fail
    }

    ul |= 4294967040u; // 0xffff_ff00 - make sure we zero-extend this to unsigned long

    if (ul != 18446460386824683288ul /* 0xfffe_fdfc_ffff_ff18 */) {
        return 2; // fail
    }

    // make sure that we convert result _back_ to type of lvalue,
    // and that we don't clobber nearby values (e.g. by trying to assign 8-byte)
    // result to four-byte ui variable
    int i = 123456;
    unsigned int ui = 4042322160u; // 0xf0f0_f0f0
    long l = -252645136; // 0xffff_ffff_f0f0_f0f0
    // 1. zero-extend ui to 8-bytes
    // 2. XOR w/ l, resulting in 0xffff_ffff_0000_0000
    // 3. truncate back to 4 bytes, resulting in 0
    // then check value of expression (i.e. value of ui)
    if (ui ^= l) {
        return 3; // fail
    }

    // check side effect (i.e. updating ui)
    if (ui) {
        return 4; // fail
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