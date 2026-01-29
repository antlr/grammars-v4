// Pass arguments of double type, including on stack, and return value of double type

double fmax(double x, double y); // from math.h

double get_max(double a, double b, double c, double d,
               double e, double f, double g, double h,
               // pass three arguments on the stack, make sure we adjust stack padding accordingly
               double i, double j, double k)
{

    double max = fmax(
        fmax(
            fmax(
                fmax(a, b),
                fmax(c, d)),
            fmax(
                fmax(e, f),
                fmax(g, h))),
        fmax(i, fmax(j, k)));
    return max;
}
