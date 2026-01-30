/* Make sure our copy propagation pass recognizes 0.0 and -0.0
 * as distinct values, even though they compare equal with standard
 * floating-point comparison operators
 * */
double copysign(double x, double y);

double target(int flag) {
    double result = 0.0;  // gen result = 0.0
    if (flag) {
        result = -0.0;  // gen result = -0.0
    }

    // can't propagate value of result because it has
    // different values on different paths
    return result;
}

int main(void) {
    double pos_inf = 1 / target(0);
    double neg_inf = 1 / target(1);
    return pos_inf > neg_inf;
}