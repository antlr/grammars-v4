/* Test constant-folding the bitwise &, |, ^, >>, and << expressions with unsigned operands */

unsigned target_and(void) {
    // 0xf0f0_f0f0 & 0xff00_ff00
    return 4042322160u & 4278255360u;
}

unsigned long target_or(void) {
    // 0x0f0f_0f0f_0f0f_0f0f | 0xff00_ff00_ff00_ff00
    return 1085102592571150095ul | 18374966859414961920ul;
}

unsigned int target_xor(void) {
    // 0xf0f0_f0f0 ^ 0x0ff0_0ff0
    return 4042322160u ^ 267390960u;
}

unsigned int target_shift_uint_left(void) {
    return 10u << 24l; // doesn't matter that right operand is different type
}

unsigned long target_shift_ulong_left(void) {
    return 2286249799ul << 33u; // result wrap arounds
}

// make sure right shift is logical, not arithmetic
unsigned int target_shift_uint_right(void) {
    return 4294967296u >> 16;
}

unsigned long target_shift_ulong_right(void) {
    return 9223372041149743104ul >> 21l;
}

int main(void) {
    if (target_and() != 4026593280u) {
        return 1;
    }

    if (target_or() != 18379189048491114255ul) {
        return 2;
    }

    if (target_xor() != 4278255360u) {
        return 3;
    }

    if (target_shift_uint_left() != 167772160u) {
        return 4;
    }

    if (target_shift_ulong_left() != 1191992160673595392ul) {
        return 5;
    }

    if (target_shift_uint_right() != 65536u) {
        return 6;
    }

    if (target_shift_ulong_right() != 4398046513152ul) {
        return 7;
    }

    return 0;
}