/* Test the -> operator.
 * Relatively simple tests without nested accesses or members of aggregate
 * types.
 */

void *calloc(unsigned long nmemb, unsigned long size);

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
    if (d != -1845381177299.0 || c != 127 || l != 58 || *ptr != 100 ||
        d_divided != -922690588 || c_doubled != 254 || l_cast != 58.0 ||
        dereferenced_ptr != 100) {
        return 0;
    }

    return 1;  // success
}

int test_auto(void) {
    // test reading, writing, and getting address of members
    // in struct with automatic storage duration
    struct four_members autom;
    struct four_members *autom_ptr = &autom;

    // write to all members - assign results of complex expression to members
    autom_ptr->d = (l - get_double()) + (l * 3.5);  // -1845381177299.0
    autom_ptr->c = 127;
    autom_ptr->l = get_double() / l;  // 58

    char chr = 100;
    autom_ptr->ptr = &chr;

    // read all members
    if (autom_ptr->d != -1845381177299.0 || autom_ptr->c != 127 ||
        autom_ptr->l != 58 || autom_ptr->ptr != &chr) {
        return 0;
    }

    // take address of members
    double *d_ptr = &autom_ptr->d;
    char *c_ptr = &autom_ptr->c;
    if (*d_ptr != -1845381177299.0 || *c_ptr != 127) {
        return 0;
    }

    // dereference member
    if (*autom_ptr->ptr != 100) {
        return 0;
    }

    // read members and use them in complex expressions (e.g. function calls)
    if (!accept_params(autom.d / 2000, autom.c * 2, (double)autom.l, *autom.ptr,
                       autom.d, autom.c, autom.l, autom.ptr)) {
        return 0;
    }

    return 1;
}

int test_static(void) {
    // test reading, writing, and getting address of members
    // in struct with static storage duration
    static struct four_members stat;
    static struct four_members *stat_ptr;
    stat_ptr = &stat;
    static char chr = 100;

    // same test as test_auto above

    // write to all members - assign results of complex expression to members
    stat_ptr->d = (l - get_double()) + (l * 3.5);  // -1845381177299.0
    stat_ptr->c = 127;
    stat_ptr->l = get_double() / l;  // 58

    stat_ptr->ptr = &chr;

    // read all members - assign results complex expression to members
    if (stat_ptr->d != -1845381177299.0 || stat_ptr->c != 127 ||
        stat_ptr->l != 58 || stat_ptr->ptr != &chr) {
        return 0;
    }

    // take address of members
    double *d_ptr = &stat_ptr->d;
    char *c_ptr = &stat_ptr->c;
    if (*d_ptr != -1845381177299.0 || *c_ptr != 127) {
        return 0;
    }

    // dereference member
    if (*stat_ptr->ptr != 100) {
        return 0;
    }

    // read members and use them in complex expressions (e.g. function calls)
    if (!accept_params(stat.d / 2000, stat.c * 2, (double)stat.l, *stat.ptr,
                       stat.d, stat.c, stat.l, stat.ptr)) {
        return 0;
    }

    return 1;  // success
}

int test_exp_result_member(void) {
    // access members through structure pointers produced by conditional,
    // assignment, and cast expressions

    static int flag = 1;

    // define/populate two structs
    struct four_members s1;
    s1.d = 10.0;
    s1.c = 99;
    s1.l = 9223372036854775807l;
    s1.ptr = 0;

    struct four_members s2;
    s2.d = 12.0;
    s2.c = 98;
    s2.l = -9223372036854775807l;
    s2.ptr = 0;

    struct four_members *s1_ptr = &s1;
    struct four_members *s2_ptr = &s2;

    // assign to member thru conditional expression
    (flag ? s1_ptr : s2_ptr)->c = 127;

    // validate
    if (s1.c != 127) {
        return 0;
    }

    if (s2.c != 98) {  // s2.c value hould be the same
        return 0;
    }

    // access member in assignment expression (and make sure assignment has
    // correct side effect)
    struct four_members *result_ptr = 0;
    // assign to result_ptr and access member through assignment expression
    if ((result_ptr = s2_ptr)->d != 12.0 ||
        // make sure we can now read other members of s2 through result_ptr too
        result_ptr->l != -9223372036854775807l) {
        return 0;
    }

    // access member through cast expression
    void *void_ptr = calloc(1, sizeof(struct four_members));
    ((struct four_members *)void_ptr)->c = 80;

    // validate
    result_ptr = void_ptr;
    if (result_ptr->c != 80) {
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

    if (!test_exp_result_member()) {
        return 3;
    }

    return 0;
}
