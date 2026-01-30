To validate that all optimization passes were run until the results converge, the test script inspects the assembly for any functions starting with `target`.  Many of the tests in this chapter focus on validating constant-folding logic that we couldn't validate before we'd implemented copy propagation (like constant folding with chararacter types and negative numbers). Others validate copy-propagation logic that we couldn't validate before implementing dead storee limination.

In one test program (`alias_analysis_change.c`) we make sure that stores of specific constants were removed, just like we do for some dead store elimination tests.

Some tests of constant folding validate that we remove all arithmetic instructions, conditional instructions, etc., similar to earlier constant-folding tests.

Some tests of copy propagation make sure that stores to a particular global variable were removed (because copy prop made a store to that variable dead) or uses of a particular global variable were removed (because copy prop replaced all uses of that variable).

In the remaining test programs, we validate that each target function is optimized down to a single `Return` instruction, much like we do for other dead store elimination tests.

To see what validateion we perform for each test program, see `test_framework/tacky/pipeline.py`.