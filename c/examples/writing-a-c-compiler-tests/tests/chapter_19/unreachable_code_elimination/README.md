To validate that dead code in a test program was eliminated, the test script inspects the assembly for the `target` function.

In most of our test programs, dead code elimination should eliminate all control-flow instructions and all but one `ret` instruction. These tests fail if `target` contains any jumps, `call` instructions, or labels, or more than one `ret`.

In a couple of programs (`dead_branch_inside_loop.c` and `dead_after_if_else.c`), dead code elimination should eliminate all function calls but not other control-flow instructions.
These tests fail if there's a `call` instruction in the `target` function.

A few programs (`keep_final_jump.c`, `empty.c`, `infinite_loop.c`, and`remove_jump_keep_label.c`) test edge cases where code _shouldn't_ be eliminated, or where a bug in unreachable code elimination is likely to just crash the compiler. The test script doesn't inspect the assembly for these; it just validates that they behave correctly.