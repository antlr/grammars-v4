/* This test case is identical to chapter13/valid/function_calls/use_arg_after_fun_call.c
 * but split across two files */
double fun(double x);

int main(void) {
    return fun(1.0);
}