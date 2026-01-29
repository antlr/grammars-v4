int main(void) {
    int x = 10;
    int *ptr = &x;
    // Make sure we can use a dereferenced pointer on the left-hand side
    // of a compound assignment expression

    // +=
    *ptr += 5;
    if (x != 15) {
        return 1;
    }

    // -=
    if ((*ptr -= 12) != 3) {
        return 2;
    }

    if (x != 3) {
        return 3;
    }

    // *=
    *ptr *= 6;
    if (x != 18) {
        return 4;
    }

    // /=
    *ptr /= 9;
    if (x != 2) {
        return 5;
    }

    // %=
    *ptr %= 3;
    if (x != 2) {
        return 6;
    }

    return 0;
}