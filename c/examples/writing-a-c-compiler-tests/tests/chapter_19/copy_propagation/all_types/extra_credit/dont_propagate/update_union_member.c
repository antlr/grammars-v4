/* Writing to union member kills previous copies to/from that union */

union u {
    long l;
    int i;
};

int main(void) {
    static union u u1 = {20};
    union u u2 = {3};
    u1 = u2; // generate u1 = u2
    u2.i = 0; // kill u1 = u2

    return u1.i;  // make sure we don't propagate u2 into this return statement
}