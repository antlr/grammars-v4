/* A non-lvalue structure that contains an array has temporary lifetime;
 * test that you can get this array's address implicitly (even though
 * you can't load it explicitly)
 * Adapted from Listing 18-27
 * */

struct s {
    int arr[3];
};

struct s f(void) {
    struct s retval = {{1, 2, 3}};
    return retval;
}

int main(void) {
    int i = f().arr[0];
    int j = f().arr[1];
    int k = f().arr[2];

    if (i != 1) {
        return 1;
    }

    if (j != 2) {
        return 2;
    }

    if (k != 3) {
        return 3;
    }
    return 0;  // success
}