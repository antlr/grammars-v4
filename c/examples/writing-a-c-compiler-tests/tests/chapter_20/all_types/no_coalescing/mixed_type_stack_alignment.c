/* Test that we adjust stack alignment to account for callee-saved registers.
 * The check_alignment function, which is written in assembly, validates that
 * RSP is aligned correctly and exits early if not.
 * The test script doesn't inspect assembly for this case, just checks the
 * exit code.
 * Similar to tests/chapter_20/int_only/no_coalescing/callee_saved_stack_alignment.c
 * but with a mix of types.
 * */

#include "../util.h" // declares check_* and id functions

 // check alignment of RSP and exit with exit_code if it's misaligned
 // defined in tests/chapter_20/helper_libs/alignment_check_<PLATFORM>.s
int check_alignment(int exit_code);

// 5 callee-saved registers, 16 bytes (2 longs) on stack
int test1(void) {
    long a = id(1);
    unsigned long b = id(2);
    long c = id(3);
    unsigned long d = id(4);
    long e = id(5);
    unsigned long f = id(6);
    long g = id(7);
    check_alignment(-1);
    check_one_int(a, 1);
    check_one_int(b, 2);
    check_one_int(c, 3);
    check_one_int(d, 4);
    check_one_int(e, 5);
    check_one_int(f, 6);
    check_one_int(g, 7);
    return 0;
}

// two callee-saved registers, 11 bytes on stack
int test2(void) {
    char a = id(4);
    unsigned int b = id(5);
    char arr[11] = { 'a', 'b', 'c', 'd','e', 'f', 'g', 'h', 'i', 'j', 'k', };
    check_alignment(-2);
    check_one_int(a, 4);
    check_one_int(b, 5);
    for (int i = 0; i < 11; i = i + 1) {
        check_one_int(arr[i], 'a' + i);
    }
    return 0;
}

// three callee-saved registers, one aliased var on the stack
int test3(void) {
    static int *ptr;
    char a = id(4);
    unsigned char b = id(5);
    long c = id(6);
    int aliased = 10;
    ptr = &aliased;
    check_alignment(-3);
    check_one_int(a, 4);
    check_one_int(b, 5);
    check_one_int(c, 6);
    check_one_int(*ptr, 10);
    return 0;
}
