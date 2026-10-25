/* Make sure a argument list ends with a closing ) and not some other character;
 * this is a regression test for a bug in the reference implementation */

int foo(int x, int y) {
    return x + y;
}

int main(void) { return foo(1, 2};}