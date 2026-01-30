/* This test case is identical to chapter13/valid/function_calls/double_parameters.c
 * but split across two files */
int check_arguments(double a, double b, double c, double d, double e, double f, double g, double h);

int main(void) {
    return check_arguments(1.0, 2.0, 3.0, 4.0, -1.0, -2.0, -3.0, -4.0);
}