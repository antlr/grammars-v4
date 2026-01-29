/* You can't declare a type with a struct specifier and then refer to it with
 * a union specifier.
 */
struct x { int a; };

int main(void) {
    union x foo; // incompatible with earlier declration of 'struct x' type
    return 0;
}