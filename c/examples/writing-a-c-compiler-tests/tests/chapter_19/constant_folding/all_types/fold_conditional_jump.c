/* Test constant folding of JumpIfZero and JumpIfNotZero instructions
 * resulting from && and || operations, with operand types other than int.
 * Identical to chapter_19/constant_folding/int_only/fold_conditional_jump.c
 * but with non-int operands
 * */
#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
#endif

// We'll emit two TACKY instructions of the form
// JumpIfZero(0, false_label)
// both should be rewritten as Jump instructions
int target_jz_to_jmp(void) {
    return 0l && 0; // 0
}

// We'll emit two TACKY instructions of the form
// JumpIfZero(1, false_label)
// both should be removed
int target_remove_jz(void) {
    return 1u && 1.; // 1
}

// We'll emit two JumpIfNotZero instructions:
// JumpIfNotZero(3, true_label)
// JumpIfNotZero(99, true_label)
// both should be written as Jump instructions
int target_jnz_to_jmp(void) {
    return 3.5 || 99ul; // 1
}

// We'll emit two JumpIfNotZero instructions:
// JumpIfNotZero(0, true_label)
// JumpIfNotZero(1, true_label)
// we should remove the first, rewrite the second as a Jump instruction
int target_remove_jnz(void) {
    return 0ul || 1; // 1
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