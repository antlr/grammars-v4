double fun(double x) {
    if (x > 2)
        return x;
    else {
        double ret = fun(x + 2); // ret = 3.0
        return ret + x; // return 4.0
    }
}
