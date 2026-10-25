// void expressions are non-scalar, so they can't be used in logical expressions

int main(void) { return 1 || (void)2; }