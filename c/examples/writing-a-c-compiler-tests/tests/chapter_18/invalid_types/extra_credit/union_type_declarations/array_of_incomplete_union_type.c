/* It's illegal to specify an array type with any incomplete
 * element type, including union types.
 */

union u;  // declare incomplete union type
int main(void) {
    // declare pointer to array of three union u elements;
    // illegal because union u is incomplete
    union u(*arr)[3];
    return 0;
}