#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
int putchar(int c);

/* Make sure we can correctly manage calling conventions from the callee side
 * (by accessing parameters, including parameters on the stack) and the caller side
 * (by calling a standard library function) in the same function
 */
int foo(int a, int b, int c, int d, int e, int f, int g, int h) {
    putchar(h);
    return a + g;
}

int main(void) {
    return foo(1, 2, 3, 4, 5, 6, 7, 65);
}