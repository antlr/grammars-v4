/* All union initializers (including nested ones) must have exactly one element */

int main(void) {
    union u {
        double d; int x;
    };
    union u array_of_unions[3] = {
        // invalid; each of these must be individually enclosed in braces
        {1.0, 2.0, 3.0}
    };
}