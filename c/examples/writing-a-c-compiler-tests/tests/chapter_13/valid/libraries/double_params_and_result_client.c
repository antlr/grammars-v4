double get_max(double a, double b, double c, double d,
               double e, double f, double g, double h,
               double i, double j, double k);

int main(void)
{
    double result = get_max(100.3, 200.1, 0.01, 1.00004e5, 55.555, -4., 6543.2,
                            9e9, 8e8, 7.6,  10e3 * 11e5);
    return result == 10e3 * 11e5;
}