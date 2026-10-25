/* Make sure stack arguments are deallocated correctly after returning from a function call; also test passing variables as stack arguments */

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int lots_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o) {
    return l + o;
}

int main(void) {
    int ret = 0;
    for (int i = 0; i < 10000000; i = i + 1) {
        ret = lots_of_args(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ret, 13, 14, 15);
    }
    return ret == 150000000;
}