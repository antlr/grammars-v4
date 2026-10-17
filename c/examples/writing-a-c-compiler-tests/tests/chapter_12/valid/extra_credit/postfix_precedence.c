/* Test that cast has lower precedence that postfix ++/--
 * This isn't specific to unsigned integers but unsigned conversions
 * make it possible to test.
 */

int main(void) {
    unsigned int ui = 4294967295U;

    // First increment, which wraps value of ui around to 0
    // (but note that value of result is ui BEFORE increment)
    // Then convert to unsigned long, which doesn't change value
    // If we cast first, it won't wrap around and value won't be 0
    if (((unsigned long)ui++) != 4294967295U) {
        return 1; // fail
    }
    if (ui) {
        return 2; // fail - ui should be 0 after update
    }
    return 0; // success
}