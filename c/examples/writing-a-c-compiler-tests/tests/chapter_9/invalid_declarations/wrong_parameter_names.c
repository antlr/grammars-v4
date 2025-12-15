int foo(int a);

int main(void) {
    return foo(3);
}

int foo(int x) {
    /* Only the parameter names from this definition are in scope.
     * Parameter names from earlier declarations of foo aren't!
     */
    return a;
}