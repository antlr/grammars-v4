To validate that copies were propagated in a test program, the test script inspects the assembly for the `target` function.

In some test programs, the copy propagation and constant folding passes make it possible to evaluate the return value at compile time. The test script expects these programs to include a `mov` instruction that copies the expected constant into EAX, followed by the function epilogue.

In some programs, copy propagation should replace the arguments to certain function calls with constants. In other programs, copy propagation should propagate the same value two different function arguments. The test script validates these programs by checking which values are copied into the parameter-passing registers before the `call` instruction.

Register coalescing, which we implement in Chapter 20, can make it look like the same value is passed in two different parameter-passing registers, even if that value wasn't propagated to both parameters. The tests are designed to prevent register coalescing in those cases, so they'll still test the intended cases after you complete Chapter 20.

In a few programs, including `redundant_copies.c`, removing a redundant copy makes a whole branch dead, allowing unreachable code elimination to remove that branch. The test script validates that these programs contains no control-flow instructions.

In `pointer_arithmetic.c`, the test script validates that we optimize away all computation instructions (e.g. arithmetic instructions like `imul` and type conversions like `movsx`).

The programs in the `dont_propagate` directories cover cases where copies should _not_ be propagated. The test script doesn't inspect the assembly for these; it just validates that they behave correctly.

To see what validation we perform for each test case, see `test_framework/tacky/copy_prop.py`.