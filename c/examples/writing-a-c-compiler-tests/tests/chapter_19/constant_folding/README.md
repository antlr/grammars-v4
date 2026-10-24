To validate that operations in a test program were constant folded,
the test script looks at the assembly code for each `target_*` function
in the program. Constant-folded `target_*` functions shouldn't contain
arithmetic instructions, conditional jumps or sets, type conversion
instructions, or other computations. They should only contain:
* The function prologue and epilogue
* `mov` and `jmp` instructions
* `xor op, op`, which is equivalent to `mov $0, op`

If a compiled `target_*` function includes any other instructions, the test fails.
The test script doesn't look at the assembly for other functions (e.g. `main`).

We avoid constant folding in `main`, where we compare the results of `target_*`
functions to their expected values, so the expected value we're comparing to
will be correct even if constant folding is buggy. For example if the result
of `target_neg()` should be `-3`, we might have:

```
int three = 3;

int main(void) {
    if (target_neg() != -three) {
        return 1;
    }
    // etc.
}
```

Because we can't infer the value of `three` in main, the expression `-three`
won't be constant folded.
