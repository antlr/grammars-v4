/* Test that we correctly track both implicit type conversions via array decay
 * and explicit casts
 */

#if defined SUPPRESS_WARNINGS && !defined __clang__
#pragma GCC diagnostic ignored "-Wtautological-compare"
#endif

int main(void) {
    long arr[4] = {-1,-2,-3,-4};

    // (long *) cast here is a no-op, since arr already decays to a pointer to its first element
    if (arr != (long *) arr) {
        return 1;
    }

    // taking address with & and explicitly converting to pointer to array
    // both result in address of arr with same type
    if ((long (*)[4]) arr != &arr) {
        return 2;
    }

    // reinterpret arr as an array of unsigned longs
    // NOTE: effective type rules usually don't let you read an object
    // with an lvalue of different type, but reading signed integer thru
    // corresponding unsigned type, and vice versa, is okay.
    unsigned long *unsigned_arr = (unsigned long *)arr;
    if (unsigned_arr[0] != 18446744073709551615UL) {
        return 3;
    }

    if (unsigned_arr[3] != 18446744073709551612UL) {
        return 4;
    }

    return 0;
}