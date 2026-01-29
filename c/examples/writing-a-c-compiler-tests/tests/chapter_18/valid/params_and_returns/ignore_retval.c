/* Test returning a struct and discarding the result
 * Make sure this works for structs passed in registers and
 * on the stack
 * */

struct small {
    int x;
};

struct big {
    double d;
    int x;
    long l;
};

struct small globl = {0};
struct small return_in_reg(void) {
    globl.x = globl.x + 1;
    return globl;
}

struct big globl2 = {1.25, 2, 300};
struct big return_in_mem(void) {
    globl2.d = globl2.d * 2;
    globl2.x = globl2.x * 3;
    globl2.l = globl2.l * 4;
    return globl2;
}

int main(void) {
    // can either explicitly cast result to void or just not use it
    (void)return_in_reg();
    return_in_reg();
    if (globl.x != 2) {
        return 1;
    }

    // do the same for struct return in memory
    return_in_mem();
    (void)return_in_mem();
    if (globl2.d != 5.0 || globl2.x != 18 || globl2.l != 4800) {
        return 2;
    }
    return 0;  // success
}