/* This test case is identical to chapter13/valid/function_calls/double_and_int_params_recursive.c
 * but split across two files */
int fun(int i1, double d1, int i2, double d2, int i3, double d3,
        int i4, double d4, int i5, double d5, int i6, double d6,
        int i7, double d7, int i8, double d8, int i9, double d9);
int main(void) {
    double d = fun(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0, 11, 12.0, 13, 14.0, 15, 16.0, 17, 18.0);
    return (d == 78.00);
}