int main(void) {
    int arr[3] = {4, 5, 6};
    /* It's illegal to use an expression of non-integer type in a subscript operation.
     * Note that this is a type error because our grammar allows arbitrary
     * expressions here, but using a double in an _array declarator_ is a parse error
     * because our grammar only allows integer constants in these declarators
     */
    return arr[2.0];
}