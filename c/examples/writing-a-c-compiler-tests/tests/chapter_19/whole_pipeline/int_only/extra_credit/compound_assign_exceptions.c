/* Make sure we don't throw an error when constant folding /= or %=
 * that involves division by zero, or when performing +=, *= or -=
 * that would overflow. Tn this program, these operations
 * will never actually be executed.
 * */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wdivision-by-zero"
#else
#pragma GCC diagnostic ignored "-Wdiv-by-zero"
#endif
#endif


static int zero;

int main(void) {
    int w = 3;
    int x = 10;
    int y = 2147483647;
    int z = -2147483647;
    if (zero) {
        w %= 0;
        x /= 0;
        y += 10;
        z -= 10;
    }
    if (w != 3) {
        return 1;
    }

    if (x != 10) {
        return 2;
    }

    if (y != 2147483647) {
        return 3;
    }

    if (z != -2147483647) {
        return 4;
    }

    return 0; // success
}