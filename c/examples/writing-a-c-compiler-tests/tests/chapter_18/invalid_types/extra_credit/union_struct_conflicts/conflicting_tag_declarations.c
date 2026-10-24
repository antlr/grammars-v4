/* It's illegal to specify struct and union types with the same tag in the same scope */
struct x;
union x;

int main(void) {
    return 0;
}