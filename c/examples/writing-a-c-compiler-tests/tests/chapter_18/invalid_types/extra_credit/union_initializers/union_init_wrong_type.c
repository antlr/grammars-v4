/* The one element in a union initializer must be compatible with the union's
 * first member.
 */

union u {
    long *ptr;
    double d;
};

int main(void) {
    // invalid; cannot implicitly convert double 1.0 to type of first
    // member (long *)
    union u my_union = {1.0};
}