/* Test constant folding of JumpIfZero and JumpIfNotZero instructions
 * resulting from && and || operations.
 * */
#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wconstant-logical-operand"
#endif

// We'll emit two TACKY instructions of the form
// JumpIfZero(0, false_label)
// both should be rewritten as Jump instructions
int target_jz_to_jmp(void) {
    return 0 && 0; // 0
}

// We'll emit two TACKY instructions of the form
// JumpIfZero(1, false_label)
// both should be removed
int target_remove_jz(void) {
    return 1 && 1; // 1
}

// We'll emit two JumpIfNotZero instructions:
// JumpIfNotZero(3, true_label)
// JumpIfNotZero(99, true_label)
// both should be written as Jump instructions
int target_jnz_to_jmp(void) {
    return 3 || 99; // 1
}

// We'll emit two JumpIfNotZero instructions:
// JumpIfNotZero(0, true_label)
// JumpIfNotZero(1, true_label)
// we should remove the first, rewrite the second as a Jump instruction
int target_remove_jnz(void) {
    return 0 || 1; // 1
}

int main(void) {
    if (target_jz_to_jmp() != 0) {
        return 1;
    }
    if (target_remove_jz() != 1) {
        return 2;
    }
    if (target_jnz_to_jmp() != 1) {
        return 3;
    }
    if (target_remove_jnz() != 1) {
        return 4;
    }
    return 0; // success
}