int f(void) {
    static int i = 0;
    static int j = 0;
    static int k = 1;
    static int l = 48;
    i += 1;
    j -= i;
    k *= j;
    l /= 2;

    // expected values after 3 invocations:
    // i = 3
    // j = -6
    // k = -18
    // l = 6
    if (i != 3) {
        return 1;
    }
    if (j != -6) {
        return 2;
    }
    if (k != -18) {
        return 3;
    }
    if (l != 6) {
        return 4;
    }
    return 0;
}

int main(void) {
    f();
    f();
    return f();
}