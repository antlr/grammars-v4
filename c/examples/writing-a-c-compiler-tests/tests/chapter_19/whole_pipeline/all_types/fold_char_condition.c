/* Test constant folding of Not, JumpIfZero, and JumpIfNotZero with char
 * operands. (We don't test constant-folding of other operations on char because
 * they get promoted to int first.)
 * */
#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif
int putchar(int c);

int target_not_char(void) {
    char x = 256;  // 0
    return !x;     // 1
}

int target_not_uchar(void) {
    unsigned char x = 256;  // 0
    return !x;              // 1
}

int target_not_true_char(void) {
    char x = -1;
    return !x;  // 0;
}

int target_and_schar(void) {
    signed char c = 0;
    return c && putchar('a');  // return 1, eliminate call to putchar
}

int target_and_true_char(void) {
    signed char c1 = 44;
    char c2 = c1 - 10;
    return c1 && c2;  // 1
}

int target_or_uchar(void) {
    unsigned char u = 250;
    return u || putchar('a');  // return 1, eliminate call to putchar
}

int target_or_char(void) {
    char c = 250;
    return c || putchar('a');  // return 1, eliminate call to putchar
}

char target_branch_char(void) {
    unsigned char u = 250;
    u = u + 6;  // 0
    if (u) {    // eliminate this branch
        putchar('a');
    }
    return u + 10;
}

int main(void) {
    if (target_not_char() != 1) {
        return 1;  // fail
    }
    if (target_not_uchar() != 1) {
        return 2;  // fail
    }
    if (target_not_true_char() != 0) {
        return 3;  // fail
    }
    if (target_and_schar() != 0) {
        return 4;  // fail
    }
    if (target_and_true_char() != 1) {
        return 5;  // fail
    }
    if (target_or_uchar() != 1) {
        return 6;  // fail
    }
    if (target_or_char() != 1) {
        return 7;  // fail
    }
    if (target_branch_char() != 10) {
        return 8;  // fail
    }
    return 0;  // success
}