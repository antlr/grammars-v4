/* Make sure we can call floating-point functions from the standard library */

/* We need to declare these functions ourselves since we can't #include <math.h> */

// fused multiply and add: (x * y) + z
// note: only the final result of the whole calculation is rounded,
// not the intermediate result x * y
double fma(double x, double y, double z);

double ldexp(double x, int exp); // x * 2^exp

int main(void) {
    double fma_result = fma(5.0, 1E22, 4000000.0);
    double ldexp_result = ldexp(92E73, 5);
    if (fma_result != 50000000000000004194304.0) {
        return 1;
    }

    if (ldexp_result != 2.944E76) {
        return 2;
    }

    return 0;
}