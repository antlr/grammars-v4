/* Test that we eliminate useless JumpIfZero and JumpIfNotZero instructions. */

#if defined SUPPRESS_WARNINGS && defined __clang__
#pragma clang diagnostic ignored "-Wconstant-logical-operand"
#endif
int target(int a) {
    // on second unreachable code elimination pass, this will include
    // a JumpIfNotZero to its default successor (where we assign result = 1)
    int x = a || 5;

    // on second unreachable code elimination pass, this will include
    // a JumpIfZero to its default successor (where we assign result = 0)
    int y = a && 0;
    return x + y;
}

int main(void) {
    return target(10);
}