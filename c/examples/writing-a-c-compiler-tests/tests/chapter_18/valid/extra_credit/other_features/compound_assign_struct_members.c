// Compound assignment operations with structure members
struct inner {
    double a;
    char b;
    int *ptr;
};

struct outer {
    unsigned long l;
    struct inner *in_ptr;
    struct inner in_array[4];
    int bar;
};

int main(void) {
    int i = -1;
    int i2 = -2;
    struct inner si = {150., -12, &i};
    struct outer o = {// l
                      18446744073709551615UL,
                      // in_ptr
                      &si,
                      // in_array
                      {si, {-20e20, 120, 0}, {0, 0, 0}, {1, 1, &i2}},
                      // bar
                      2000};

    // +=
    si.a += 10;  // 150. + 10 = 160
    if (si.a != 160) {
        return 1;  // fail
    }

    // -=
    // no overflow b/c of integer promotion
    o.in_array[0].b -= 460;  //  -12 - 460 = -472, reduces to 40
    if (o.in_array[0].b != 40) {
        return 2;  // fail
    }

    // *=
    o.in_array[1].a *= -4;  // -20e20 * -4 = 80e20
    if (o.in_array[1].a != 80e20) {
        return 4;  // fail
    }

    // /=
    o.in_ptr->a /= 5;  // 160. / 5 = 32
    // o.in_ptr points to si
    if (si.a != 32) {
        return 5;  // fail
    }

    // %=
    (&o)->l %= o.bar;  // 18446744073709551615 % 2000 = 1615
    if (o.l != 1615) {
        return 6;  // fail
    }

    // pointer +=
    o.in_ptr = o.in_array;
    if ((o.in_ptr += 3)->a != 1) {
        return 7;  // fail
    }
    if (*o.in_ptr->ptr != -2) {
        return 8;  // fail
    }

    // pointer -=
    o.in_ptr -= 1u;
    if (o.in_ptr->a || o.in_ptr->b || o.in_ptr->ptr) {
        return 9;  // fail
    }

    // validate everything! (make sure nothing was clobbered)
    if (si.a != 32 || si.b != -12 || si.ptr != &i) {
        return 10;  // fail
    }

    if (o.l != 1615) {
        return 11;  // fail
    }

    if (o.in_ptr != &o.in_array[2]) {
        return 12;  // fail
    }

    if (o.in_array[0].a != 150. || o.in_array[0].b != 40 ||
        o.in_array[0].ptr != &i) {
        return 13;  // fail
    }

    if (o.in_array[1].a != 80e20 || o.in_array[1].b != 120 ||
        o.in_array[1].ptr) {
        return 14;  // fail
    }

    if (o.in_array[2].a || o.in_array[2].b || o.in_array[2].ptr) {
        return 15;  // fail
    }

    if (o.in_array[3].a != 1 || o.in_array[3].b != 1 ||
        o.in_array[3].ptr != &i2) {
        return 16;  // fail
    }

    if (o.bar != 2000) {
        return 17;
    }

    return 0;
}