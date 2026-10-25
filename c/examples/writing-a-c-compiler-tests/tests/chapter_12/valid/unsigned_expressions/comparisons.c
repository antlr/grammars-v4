/* Test comparisons of unsigned integers */

unsigned int one_hundred = 100u;
unsigned int large_uint = 4294967294u; // interpreted as a signed int, this would be -2

unsigned long one_hundred_ulong = 100ul;
unsigned long large_ulong = 4294967294ul; // this would have the same value as a signed long

int main(void) {
    // compare unsigned ints (result would be different if interpreted as signed)

    /* False comparisons */
    if (large_uint < one_hundred)
        return 1;
    if (large_uint <= one_hundred)
        return 2;
    if (one_hundred >= large_uint)
        return 3;
    if (one_hundred > large_uint)
        return 4;
    /* True comparisons */
    if (!(one_hundred <= large_uint))
        return 5;
    if (!(one_hundred < large_uint))
        return 6;
    if (!(large_uint > one_hundred))
        return 7;
    if (!(large_uint >= one_hundred))
        return 8;

    // compare unsigned longs (result would be the same if interpreted as signed)
    /* False comparisons: */
    if (large_ulong < one_hundred_ulong)
        return 9;
    if (large_ulong <= one_hundred_ulong)
        return 10;
    if (one_hundred_ulong >= large_ulong)
        return 11;
    if (one_hundred_ulong > large_ulong)
        return 12;
    /* True comparisons */
    if (!(one_hundred_ulong <= large_ulong))
        return 13;
    if (!(one_hundred_ulong < large_ulong))
        return 14;
    if (!(large_ulong > one_hundred_ulong))
        return 15;
    if (!(large_ulong >= one_hundred_ulong))
        return 16;

    return 0;
}
