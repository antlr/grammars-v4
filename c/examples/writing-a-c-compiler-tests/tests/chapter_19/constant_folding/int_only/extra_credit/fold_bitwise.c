/* Test constant-folding the bitwise &, |, ^, >>, and << expressions */

int target_and(void) {
    // 0x0f0f_0f0f & 0x00ff_00ff
    return 252645135 & 16711935;
}

int target_or(void) {
    // 0x0f0f_0f0f | 0x00ff_00ff
    return 252645135 | 16711935;
}

int target_xor(void){
    // 0x0f0f_0f0f ^ 0x00ff_00ff
    return 252645135 ^ 16711935;
}

int target_shift_left(void) {
    return 291 << 18;
}

int target_shift_right(void) {
    return 252645135 >> 9;
}

int main(void) {
    if (target_and() != 983055) {
        return 1;
    }

    if (target_or() != 268374015) {
        return 2;
    }

    if (target_xor() != 267390960) {
        return 3;
    }

    if (target_shift_left() != 76283904) {
        return 4;
    }

    if (target_shift_right() != 493447){
        return 5;
    }

     return 0;
}