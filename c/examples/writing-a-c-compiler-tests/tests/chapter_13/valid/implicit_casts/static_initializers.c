/* Test initializing static doubles with integer constants and vice versa */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wimplicit-const-int-float-conversion"
#pragma GCC diagnostic ignored "-Wliteral-conversion"
#endif
#endif

// double variables

// can convert from int/uint without rounding
double d1 = 2147483647;
double d2 = 4294967295u;

/* midway point between 4611686018427388928.0 and 4611686018427389952.0
 * We round ties to even, so round this up to 4611686018427389952.0
 */
double d3 = 4611686018427389440l;

/* We'll round this down to 4611686018427389952.0 */
double d4 = 4611686018427389955l;

/* Using round-to-nearest, this rounds to 9223372036854775808 */
double d5 = 9223372036854775810ul;
double d6 = 4611686018427389955ul; // this as the same value as d4 and should round to the same double

/* This is exactly halfway between 9223372036854775808.0 and
 * 9223372036854777856.0 We round ties to even, so this
 * rounds down to 9223372036854775808.0
 */
double d7 = 9223372036854776832ul;

double uninitialized; // should be initialized to 0.0

// integer variables

static int i = 4.9; // truncated to 4

int unsigned u = 42949.672923E5; // truncated to 4294967292u

// this token is first converted to a double w/ value 4611686018427389952.0,
// then truncated down to long 4611686018427389952
long l = 4611686018427389440.;

unsigned long ul = 18446744073709549568.;

int main(void) {
    if (d1 != 2147483647.) {
        return 1;
    }

    if (d2 != 4294967295.) {
        return 2;
    }
    if (d3 != 4611686018427389952.) {
        return 3;
    }

    if (d4 != d3) {
        return 4;
    }

    if (d5 != 9223372036854775808.) {
        return 5;
    }

    if (d6 != d3) {
        return 6;
    }

    if (d7 != d5) {
        return 7;
    }

    if (uninitialized) {
        return 8;
    }

    if (i != 4) {
        return 9;
    }

    if (u != 4294967292u) {
        return 10;
    }

    if (l != 4611686018427389952l) {
        return 11;
    }

    if (ul != 18446744073709549568ul) {
        return 12;
    }

    return 0;
}