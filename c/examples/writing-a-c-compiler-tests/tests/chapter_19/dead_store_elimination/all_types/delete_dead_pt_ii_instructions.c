/* Make sure we can delete type conversions, load, and other instructions
 * from Part II when they're dead stores
 * */

#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

long l = 1l;
int i = 2;
unsigned int u = 30u;

struct s {
    int a;
    int b;
    int c;
};

int target(void) {
    // everything except the return instruction should be deleted
    long x = (long) i; // dead sign extend
    unsigned long y = (unsigned long) u; // dead zero extend
    double d = (double) y + (double) i; // dead IntToDouble and UIntToDouble
    x = (long) d; // dead DoubleToInt
    y = (unsigned long) d; // dead DoubleToUInt
    int arr[3] = {1, 2, 3}; // dead CopyToOffset
    int j = arr[2]; // dead AddPtr and Load
    int *ptr = &i; // dead GetAddress
    char c = (char)l; // dead truncate
    struct s my_struct = {0, 0, 0};
    j = my_struct.b; // dead CopyFromOffset
    d = -d * 5.0; // dead Binary/Unary instructions w/ non-int operands
    return 5;
}

int main(void) {
    return target();
}