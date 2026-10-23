// the element type in an array declarator must be complete,
// so specifying a parameter with type void[3] is illegal,
// even though it would be adjusted
// to the valid type void *
int arr(void foo[3]) { return 3; }

int main(void) { return 0; }