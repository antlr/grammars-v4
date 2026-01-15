/* Verify that you can call a function that's declared
 * with external linkage in the current file,
 * and define with external linkage in another file.
 */

int add_one_and_two(void) {
    /* You can declare a function multiple times at block scope;
     * extern keyword doesn't matter */
    extern int sum(int a, int b);
    int sum(int a, int b);
    return sum(1, 2);
}

/* You can declare a function multiple times at file scope;
 * extern keyword still doesn't matter
 */
extern int sum(int x, int y);
int sum(int x, int y);


int add_three_and_four(void) {
    /* Define a sum variable shadowing the sum function */
    int sum = 3;
    if (sum > 2) {
        /* The extern keyword can bring a shadowed
         * function identifier back into scope
         */
        extern int sum(int one, int two);
        return sum(3, 4);
    }
    return 1;
}

int main(void) {
    if (add_three_and_four() != 7)
        return 1;
    if (add_one_and_two() != 3)
        return 1;
    return 0;
}