int bar(void) {
    return 9;
}

int foo(void) {
    return 2 * bar();
}

int main(void) {
    /* Use multiple function calls in an expression,
     * make sure neither overwrites the other's return value in EAX */
    return foo() + bar() / 3;
}