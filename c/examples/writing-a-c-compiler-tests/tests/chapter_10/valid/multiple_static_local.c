/* Multiple functions may declare static local variables
 * with the same name; these variables have no linkage,
 * and are distinct from each other.
 */

int foo(void) {
    /* 'a' is a static local variable.
     * its value doubles each time we call foo()
     */
    static int a = 3;
    a = a * 2;
    return a;
}

int bar(void) {
    /* 'a' is a static local variable, distinct from the
     * 'a' variable declared in foo.
     * its value increases by one each time we call bar()
     */
    static int a = 4;
    a = a + 1;
    return a;
}

int main(void) {
    return foo() + bar() + foo() + bar();
}