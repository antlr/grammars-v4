/* Test case where result of casting double to integer is undefined (because
 * the result is out of range). The program's behavior is well-defined
 * because the cast operations aren't actually executed; the main thing we're
 * testing here is that compiler doesn't crash during the constant folding pass.
 * There are no target_ functions
 * because we don't inspect the assembly in this program.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wliteral-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif


int main(void) {
    int dead_int_cast = 0 ? (int)2147483649.0 : 100; // in the range of uint but not int
    unsigned int dead_uint_cast = 0 ? (unsigned int) 34359738368.0 : 200; // in the range of long but not uint
    signed long dead_long_cast = 1 ? 300 : 9223372036854777856.0; // in the range of unsigned long but not long
    unsigned long dead_ulong_cast = 1 ? 200 : (unsigned long)200e300; //outside the range of unsigned long
    return dead_int_cast + dead_uint_cast + dead_long_cast + dead_ulong_cast;
}