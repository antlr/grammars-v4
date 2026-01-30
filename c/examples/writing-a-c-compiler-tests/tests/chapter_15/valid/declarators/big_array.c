/* Test that we can parse an array declarator with a size greater than UINT_MAX
 * Note that we don't actually allocate space for this array!
 */

extern int x[4294967297L][100000000];

int main(void) {
    return 0;
}