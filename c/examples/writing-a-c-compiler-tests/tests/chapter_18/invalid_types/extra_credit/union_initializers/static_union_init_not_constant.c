/* The initializer for any static variable must be a constant,
 * including variables of union type.
 */

union u {int a; int b;};

int main(void){
    int i = 10;
    static union u my_union = {i};
    return 0;
}