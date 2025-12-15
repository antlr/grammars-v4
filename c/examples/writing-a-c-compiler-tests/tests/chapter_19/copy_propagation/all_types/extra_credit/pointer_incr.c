/* We can calculate constant offset for ++/-- with pointers into arrays;
 * similar to pointer_arithmetic.c
 */
int target(void) {
    int nested[3][23] = { {0, 1}, {2} };
    int (* ptr)[23] = nested;
    ptr++;
    return *ptr[0];
}

int main(void) {
    return target();
}