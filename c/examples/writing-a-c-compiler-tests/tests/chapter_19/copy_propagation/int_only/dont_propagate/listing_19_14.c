/* Test that function calls kill copies to static local variables;
 * example from Listing 19-14
 * */

int indirect_update(void);

int f(int new_total) {
    static int total = 0;
    total = new_total;  // generate copy total = new_total
    if (total > 100)
        return 0;
    total = 10;         // generate total = 10
    indirect_update();  // kill total = 10 (b/c total is static)
    return total;       // can't rewrite as 'return 10'
}

int indirect_update(void) {
    f(101);  // this will update 'total'
    return 0;
}

int main(void) {
    return f(1);  // expected return value: 101
}