long sign_extend(int i, long expected) {
    long extended = (long) i;
    return (extended == expected);
}


int main(void) {
    /* Converting a positive or negative int to a long preserves its value */
    if (!sign_extend(10, 10l)) {
        return 1;
    }

    if (!sign_extend(-10, -10l)) {
        return 2;
    }

    /* sign-extend a constant to make sure we've implemented rewrite rule for movsx correctly */
    long l = (long) 100;
    if (l != 100l) {
        return 3;
    }
    return 0;
}