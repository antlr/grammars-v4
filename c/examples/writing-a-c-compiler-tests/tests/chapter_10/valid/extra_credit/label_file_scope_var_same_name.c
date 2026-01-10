// file scope variables and labels are in different namespaces,
// so they can share the same name

int x;  // file scope var - initialized to 0

int main(void) {
    int x = 10;  // declare a local var
    goto x;      // refers to label, not either variable
    return x;
    {
        // bring global var back into scope, shadowing local one
        // NOTE: this also tests that we correctly resolve variable names
        // even when we jump over their declarations
        extern int x;
    x:             // label
        return x;  // global var - should return 0
    }
}