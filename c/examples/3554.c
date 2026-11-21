typedef void (*func_t)(const char *s);

func_t foo(void);

void bar_impl(const char *s) { }

func_t foo(void) {
    return bar_impl;
}

int main(void) {
    foo()("bar");
    return 0;
}
