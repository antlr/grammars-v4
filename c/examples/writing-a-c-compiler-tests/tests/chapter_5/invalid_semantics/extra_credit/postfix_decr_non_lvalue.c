int main(void) {
    int a = 10;
    // This is a semantic error, not a parser error, because it's syntactically
    // valid to apply an arbitrary number of postfix operations to an expression.
    // This is important so we can later support expressions like a++[0]
    return a++--;
}