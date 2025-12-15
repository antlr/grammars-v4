int main(void) {
    /* The abstract declarator (int (void)) is malformed:
     * we can't parse abstract function declarators.
     * In a fully C standard-compliant implementation,
     * this would be a type error rather than a parser error:
     * "int (void)" would be valid declarator for a function
     * with no parameters that returns an int,
     * but you can't cast an expression to a function type.
     */
    (int (void)) 0;
    return 0;
}