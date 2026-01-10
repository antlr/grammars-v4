/* In this function, the parameter x and
 * the return value are both passed in XMM0.
 * We make a recursive call to fun, then use x,
 * to make sure that x's value is preserved
 * across the function call.
 */
double fun(double x) {
    if (x > 2)
        return x;
    else {
        double ret = fun(x + 2); // ret = 3.0
        return ret + x; // return 4.0
    }
}

int main(void) {
    return fun(1.0);
}