/* Test that we correctly propagate copies into type conversion instructions */

int target(void) {
    unsigned char uc = 250;
    int i = uc * 2;              // 500 - tests ZeroExtend
    double d = i * 1000.;        // 500000.0 - tests IntToDouble
    unsigned long ul = d / 6.0;  // 83333 - tests DoubleToUInt
    d = ul + 5.0;                // 83338 - tests UIntToDouble
    long l = -i;                 // -500 - tests SignExtend
    char c = l;                  // 12 - tests Truncate
    return d + i - c;            // 83826 - tests DoubleToInt
}

int main(void) {
    if (target() != 83826) {
        return 1; // fail
    }
    return 0; // success
}