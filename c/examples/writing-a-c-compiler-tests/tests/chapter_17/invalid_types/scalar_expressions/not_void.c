// void expressions are non-scalar, so they can't be used in logical expressions

void f(void);
void g(void);
int main(void) { return !(1 ? f() : g()); }