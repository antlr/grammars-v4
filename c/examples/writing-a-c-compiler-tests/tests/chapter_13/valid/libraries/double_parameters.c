int check_arguments(double a, double b, double c, double d, double e, double f, double g, double h) {
    if (a != 1.0) {
        return 1;
    }
    if (b != 2.0) {
        return 2;
    }
    if (c != 3.0) {
        return 3;
    }
    if (d != 4.0) {
        return 4;
    }
    if (e != -1.0) {
        return 5;
    }
    if (f != -2.0) {
        return 6;
    }
    if (g != -3.0) {
        return 7;
    }
    if (h != -4.0) {
        return 8;
    }
    return 0;
}