/* Make sure static initializers are set to the correct
 * implicitly-converted value at program startup
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif

// this is 2^60 + 2^31 + 12
// should be truncated to 2^31 + 12 (which is 2147483660)
unsigned int u = 1152921506754330636l;

/* This should be initialized to -2147483646,
 * or 2147483650 - 2^32
 */
int i = 2147483650u;

/* This should be initialized to -9223372036854775716,
 * or 9223372036854775900 - 2^64
 */
long l = 9223372036854775900u; // note: this has type unsigned long

// this can be converted to a long with no change in value
long l2 = 2147483650u;

// any unsigned int can be converted to an unsigned long w/ no change in value
unsigned long ul = 4294967294u;

/* any signed long _literal_ can be converted to an unsigned long w/ no change in value
 * (we don't support negation expressions in constant initializers) */
unsigned long ul2 = 9223372036854775798l;

// truncate ulong 2**63 + 2**31 + 150
// to int -2**31 + 150 (which is -2147483498)
int i2 = 9223372039002259606ul;

// truncate ulong 2**63 + 2**31 + 150
// to uint 2**31 + 150 (which is 2147483798)
unsigned ui2 = 9223372039002259606ul;

int main(void)
{
    if (u != 2147483660u)
        return 1;
    if (i != -2147483646)
        return 2;
    if (l != -9223372036854775716l)
        return 3;
    if (l2 != 2147483650l)
        return 4;
    if (ul != 4294967294ul)
        return 5;
    if (ul2 != 9223372036854775798ul)
        return 6;
    if (i2 != -2147483498)
        return 7;
    if (ui2 != 2147483798u)
        return 8;
    return 0;
}