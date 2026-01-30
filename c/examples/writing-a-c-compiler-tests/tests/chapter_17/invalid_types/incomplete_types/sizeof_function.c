int x(void) { return 0; }

// can't apply sizeof to a function
int main(void) { return sizeof x; }