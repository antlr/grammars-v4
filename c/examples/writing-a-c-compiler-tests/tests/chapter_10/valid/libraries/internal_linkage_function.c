/* A function with internal linkage can be declared multiple times */
static int my_fun(void);

int call_static_my_fun(void) {
    return my_fun();
}

int call_static_my_fun_2(void) {
    /* when you declare a function at block scope,
     * it takes on the linkage of already-visible declaration
     */
    int my_fun(void);
    return my_fun();
}

extern int my_fun(void);

static int my_fun(void);

int my_fun(void) {
    static int i = 0;
    i = i + 1;
    return i;
}