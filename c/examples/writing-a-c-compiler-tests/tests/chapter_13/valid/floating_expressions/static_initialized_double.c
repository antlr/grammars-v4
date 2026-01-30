// Test reading and writing a local static double

// Return old value, then increment by one
double return_static_variable(void) {
    static double d = 0.5;
    double ret = d;
    d = d + 1.0;
    return ret;
}

int main(void) {
    double d1 = return_static_variable();
    double d2 = return_static_variable();
    double d3 = return_static_variable();
    if (d1 != 0.5) {
        return 1;
    }
    if (d2 != 1.5) {
        return 2;
    }
    if (d3 != 2.5) {
        return 3;
    }
    return 0;
}
