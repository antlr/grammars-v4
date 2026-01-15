/* You can cast a struct to void,
 * even though you can't cast it to any other type
 * */

struct s {
    int a;
    int b;
};

int main(void) {
    struct s x = {1, 2};
    (void)x;  // just make sure this doesn't cause a type error
    return 0;
}