/* Test the . operator.
 * Relatively simple tests without nested accesses or members of aggregate
 * types.
 */

struct four_members {
    double d;
    char c;
    long l;
    char *ptr;
};

// helper functions/variables

// get_double and l are used to initialize members
double get_double(void) {
    return 2e12;
}

static long l = 34359738378l;

// validate members (and values derived from members) that are passed as
// parameters
int accept_params(int d_divided, int c_doubled, double l_cast,
                  int dereferenced_ptr, double d, int c, long l, char *ptr) {
    if (d != 4e12 || c != 127 || l != 8589934594l || *ptr != 100 ||
        d_divided != 100.0 || c_doubled != 254 || l_cast != 8589934594.0 ||
        dereferenced_ptr != 100) {
        return 0;
    }

    return 1;  // success
}

int test_auto(void) {
    // test reading, writing, and getting address of members
    // in struct with automatic storage duration
    struct four_members autom;

    // write to all members - assign results of complex expression to members
    autom.d = get_double() * 2.0;  // 4e12
    autom.c = 127;
    autom.l = l / 4;  // 8589934594l

    char chr = 100;
    autom.ptr = &chr;

    // read all members
    if (autom.d != 4e12 || autom.c != 127 || autom.l != 8589934594l ||
        autom.ptr != &chr) {
        return 0;
    }

    // take address of members
    double *d_ptr = &autom.d;
    char *c_ptr = &autom.c;
    if (*d_ptr != 4e12 || *c_ptr != 127) {
        return 0;
    }

    // dereference member
    if (*autom.ptr != 100) {
        return 0;
    }

    // read members and use them in complex expressions (e.g. function calls)
    if (!accept_params(autom.d / 4e10, autom.c * 2, (double)autom.l, *autom.ptr,
                       autom.d, autom.c, autom.l, autom.ptr)) {
        return 0;
    }

    return 1;
}

int test_static(void) {
    // test reading, writing, and getting address of members
    // in struct with static storage duration
    static struct four_members stat;
    static char chr = 100;

    // same test as test_auto above

    // write to all members - assign results of complex expression to members
    stat.d = get_double() * 2.0;  // 4e12
    stat.c = 127;
    stat.l = l / 4;  // 8589934594l

    stat.ptr = &chr;

    // read all members
    if (stat.d != 4e12 || stat.c != 127 || stat.l != 8589934594l ||
        stat.ptr != &chr) {
        return 0;
    }

    // take address of members
    double *d_ptr = &stat.d;
    char *c_ptr = &stat.c;
    if (*d_ptr != 4e12 || *c_ptr != 127) {
        return 0;
    }

    // dereference member
    if (*stat.ptr != 100) {
        return 0;
    }

    // read members and use them in complex expressions (e.g. function calls)
    if (!accept_params(stat.d / 4e10, stat.c * 2, (double)stat.l, *stat.ptr,
                       stat.d, stat.c, stat.l, stat.ptr)) {
        return 0;
    }

    return 1;  // success
}

int main(void) {
    // accessing struct w/ automatic storage duration
    if (!test_auto()) {
        return 1;
    }

    // accessing struct w/ static storage duration
    if (!test_static()) {
        return 2;
    }

    return 0;
}
