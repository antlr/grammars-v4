/* A test case that takes even longer than fig_19_8.c to converge;
 * some blocks need to be visited three times before the algorithm converges.
 * */

static int outer_flag = 0;
static int inner_flag = 1;

// functions to validate args and control number of loop iterations
int inner_loop1(int a, int b, int c, int d, int e, int f) {
    // this should be the second loop iteration, so b, c, and e should be
    // updated, but a, d, and f shouldn't
    if (a != 1 || b != 11 || c != 12 || d != 4 || e != 20 || f != 100) {
        return 0;  // fail
    }
    return 1;  // success
}

int inner_loop2(int a, int b, int c, int d, int e, int f) {
    if (outer_flag == 0) {
        // first call: no variables have been updated
        if (a != 1 || b != 2 || c != 3 || d != 4 || e != 5 || f != 100) {
            return 0;  // fail
        }
    } else {
        // second call: a, b, c, and e have been updated
        if (a != 10 || b != 11 || c != 12 || d != 4 || e != 20 || f != 100) {
            return 0;  // fail
        }
    }

    return 1;  // success
}

int inner_loop3(int a, int b, int c, int d, int e, int f) {
    if (outer_flag == 0) {
        if (inner_flag == 2) {
            // first call to this function: only b has been updated
            if (a != 1 || b != 11 || c != 3 || d != 4 || e != 5 || f != 100) {
                return 0;  // fail
            }
        } else {
            // second iteration through inner loop: b and c have been updated
            if (a != 1 || b != 11 || c != 12 || d != 4 || e != 5 || f != 100) {
                return 0;  // fail
            }
        }
    } else {
        // second time through outer loop: a, b, c, and e have been updated
        if (a != 10 || b != 11 || c != 12 || d != 4 || e != 20 || f != 100) {
            return 0;  // fail
        }
    }

    return 1;  // success
}

int inner_loop4(int a, int b, int c, int d, int e, int f) {
    // this never runs
    // use all parameters to silence compiler warnings
    return a + b + c + d + e + f;
}

int validate(int a, int b, int c, int d, int e, int f) {
    // a, b, c, and e have been updated
    if (a != 10 || b != 11 || c != 12 || d != 4 || e != 20 || f != 100) {
        return 0;  // fail
    }
    return 1;  // success
}

int target(void) {
    // we can propagate f throughout whole function, but nothing else
    int a = 1;
    int b = 2;
    int c = 3;
    int d = 4;
    int e = 5;
    int f = 100;

    // go through outer loop twice
    while (outer_flag < 2) {
        // skip this loop on first outer iteration, run on second
        while (inner_flag < 1) {
            if (!inner_loop1(a, b, c, d, e, f)) {
                return 1;  // fail
            }
            a = 10;
            inner_flag = 1;
        }

        // do this loop once per outer iteration
        while (inner_flag < 2) {
            if (!inner_loop2(a, b, c, d, e, f)) {
                return 2;  // fail
            }
            b = 11;
            // set inner_flag to 2 so this loop doesn't run again but next loop
            // runs twice
            inner_flag = 2;
        }

        // do this loop twice per outer iteration
        while (inner_flag < 4) {
            if (!inner_loop3(a, b, c, d, e, f)) {
                return 3;  // fail
            }
            // increment inner_flag so this loop runs twice
            inner_flag = inner_flag + 1;
            c = 12;
        }

        // skip this loop both times
        while (inner_flag < 4) {
            inner_loop4(a, b, c, d, e, f);
            d = 13;
        }

        e = 20;
        f = 100;
        outer_flag = outer_flag + 1;
        // reset inner flag
        inner_flag = 0;
    }

    if (!validate(a, b, c, d, e, f)) {  // we can propagate f into this call
        return 4;
    }

    return 0;  // success
}

int main(void) {
    return target();
}