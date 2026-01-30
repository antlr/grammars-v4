// Bitwise operations with structure members

struct inner {
    char b;
    unsigned int u;
};

struct outer {
    unsigned long l;
    struct inner *in_ptr;
    int bar;
    struct inner in;
};

int main(void) {
    struct inner i = {'a', 100000u};
    struct outer o = {9223372036854775810ul, &i, 100, {-80, 4294967295U}};

    if ((i.b | o.l) != 9223372036854775907ul) {
        return 1;  // fail
    }

    if ((o.bar ^ i.u) != 100036u) {
        return 2;  // fail
    }

    if ((o.in_ptr->b & o.in.b) != 32) {
        return 3;  // fail
    }

    if ((o.l >> 26) != 137438953472ul) {
        return 4;  // fail
    }

    o.bar = 12;
    if ((i.b << o.bar) != 397312) {
        return 5;
    }

    return 0;
}
