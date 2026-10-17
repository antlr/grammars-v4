int main(void) {
    // In our implementation, this fails tag resolution because it specifies
    // an incomplete union type.
    // In a fully conforming implementation, this would fail because it defines
    // a variable with incomplete type.

    union s var;
    return 0;
}