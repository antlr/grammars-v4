/* Basic test that updating the source of a copy kills that copy */

int x = 10;

int main(void) {
    int y = x;      // generate y = x
    x = 4;          // kill y = x
    if (y != 10) {  // can't replace y with x here
        return 1;
    }
    if (x != 4) {
        return 2;
    }
    return 0;  // success
}