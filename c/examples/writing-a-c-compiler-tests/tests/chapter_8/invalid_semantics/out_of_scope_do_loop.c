/* Test case from Listing 8-3:
 * A variable declared in a loop body is out of scope in the controlling expression
 */

int main(void) {
    do {
        int a = a + 1;
    } while (a < 100);
}