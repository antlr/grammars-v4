/* Make sure we can constant fold ++/-- operations on doubles */

int target(void) {

    double d1 = 9007199254740991.0;
    double d2 = d1++; // 9007199254740992.0;

    // value of d1/d3 will still be 9007199254740992.0;
    // next representable value is 9007199254740994.0
    double d3 = ++d1;

    double e1 = 10.0;
    double e2 = --e1;
    double e3 = e1--;

    if (!(d1 == 9007199254740992.0 && d2 == 9007199254740991.0 && d1 > d2 && d1 == d3)) {
        return 1; // fail
    }

    if (!(e1 == 8. && e2 == 9. && e3 == 9.)) {
        return 2; // fail
    }

    return 0; // success
}

int main(void) {
    return target();
}