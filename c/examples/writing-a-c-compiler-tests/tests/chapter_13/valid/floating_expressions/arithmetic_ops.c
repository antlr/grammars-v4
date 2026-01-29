/* Test addition, subtraction, multiplication, division, and negation with doubles */

double point_one = 0.1;
double point_two = 0.2;
double point_three = 0.3;

double two = 2.0;
double three = 3.0;
double four = 4.0;
double twelveE30 = 12e30;

int addition(void) {
    return (point_one + point_two == 0.30000000000000004);
}

int subtraction(void) {
    return (four - 1.0 == 3.0);
}

int multiplication(void) {
    return (0.01 * point_three == 0.003);
}

int division(void) {
    return (7.0 / two == 3.5);
}

int negation(void) {
    double neg = -twelveE30;
    return !(12e30 + neg);
}

int complex_expression(void) {
    /* Test a more complex expression.
     * Note: all intermediate results in this expression
     * can be represented exactly, so we don't need to
     * consider the impact of rounding intermediate results.
     */

    double complex_expression = (two + three) - 127.5 * four;
    return complex_expression == -505.0;
}

int main(void) {

    if (!addition()) {
        return 1;
    }

    if (!subtraction()){
        return 2;
    }

    if (!multiplication()) {
        return 3;
    }

    if (!division()) {
        return 4;
    }

    if (!negation()) {
        return 5;
    }

    if (!complex_expression()) {
        return 5;
    }

    return 0;
}