/* Test that we can avoid spills in a function with a clique of fourteen
 * floating-point pseudoregisters. This tests our support for floating-point
 * register allocation in general, AND tests that we recognize that XMM0 is
 * NOT live at exit from a function with an integer return type. (If the
 * allocator thinks XMM0 is live at exit, it won't be able to assign a
 * pseudoregister to it and we'll will have to spill.)
 * The test script validates that there are no spills in the target function.
 *
 * This test program is generated from templates/chapter_20_templates/fourteen_pseudos_interfere.c.jinja.
 * */
double glob = 20.0;
double glob2 = 30.0;
int glob3 = 40.0;

int target(void) {
    // Create a clique of 14 tmps that interfere;
    // we can color all of them w/out spilling anything.
    double a = glob * glob;
    double b = glob2 + 2.0;
    double c = a + 5.0;
    double d = b - glob3;
    double e = glob + 7.0;
    double f = glob2 * 2.0;
    double g = c * 3.0;
    double h = d * 112.;
    double i = e / 3.0;
    double j = g + f;
    double k = h - j;
    double l = i + 1000.;
    double m = j - d;
    double n = m * l;

    if (a == 400.0 && b == 32.0 && c == 405.0 && d == -8.0 && e == 27.0 &&
        f == 60.0 && g == 1215.0 && h == -896. && i == 9.0 && j == 1275. &&
        k == -2171. && l == 1009. && m == 1283. && n == 1294547.) {
        return 0; // success
    } else {
        return 1; // fail
    }

}