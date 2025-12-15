// a shim for tests where the target function (dbl_target) returns a double,
// since wrapper script expects a 'target' function that returns an integer
double dbl_target(double one, double two, double three, double four, double five, double six, double seven, double eight);

int target(void) {

    return (int) dbl_target(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
}