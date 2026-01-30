static int foo = 3;

int main(void) {
    return foo;
}

/* if you declare a variable with the 'extern' keyword when a declaration
 * with linkage is already in scope, the new declaration takes on the same
 * linkage as the previous declaration
 */
extern int foo;