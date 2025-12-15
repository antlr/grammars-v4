/* Propagate ++/-- with unsigned integers (make sure they wrap around correctly) */

int target(void) {
    unsigned int u = 0;
    unsigned int u2 = --u;
    unsigned int u3 = u--;

    unsigned int u4 = 4294967295U;
    unsigned int u5 = u4++;
    unsigned int u6 = ++u4;

    if (!(u == 4294967294U && u2 == 4294967295U && u3 == 4294967295U)) {
        return 1; // fail
    }

    if (!(u4 == 1 && u5 == 4294967295U && u6 == 1)) {
        return 2; // fail
    }

    return 0; // success
}

int main(void) {
    return target();

}