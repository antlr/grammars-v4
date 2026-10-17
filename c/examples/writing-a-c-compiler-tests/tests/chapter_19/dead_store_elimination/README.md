To validate that dead stores were eliminated, the test script inspects the assembly for the `target` function.

In most programs, dead store elimination should remove a `Copy` of the form `var = const`, so the test script just validates that that constant doesn't appear in the program. In a few cases (`simple.c`, `delete_arithmetic_ops.c` and `delete_dead_pt_ii_instructions.c`), dead store elimination combined with other optimizations should eliminate the whole function body except the `Return` instruction. In these cases, the function body should contain only the prologue, a `mov` instruction that moves the expected
constant into EAX, and the epilogue.

The test cases in the `dont_elim` directories cover cases where stores _shouldn't_ be eliminated. The test script doesn't inspect the assembly for these; it just validates that they behave correctly.