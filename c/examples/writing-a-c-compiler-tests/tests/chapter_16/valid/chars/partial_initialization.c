/* Test that in arrays of character type, elements that aren't explicitly
 * initialized are zeroed out */

char static1[4] = {1, 2};
signed char static2[4] = {3, 4};
unsigned char static3[3] = {5};

int main(void)
{

    // validate static arrays
    if (static1[0] != 1 || static1[1] != 2 || static1[2] || static1[3])
        return 1;

    if (static2[0] != 3 || static2[1] != 4 || static2[2] || static2[3])
        return 2;

    if (static3[0] != 5 || static3[1] || static3[2])
        return 3;

    // define some non-static arrays
    char auto1[5] = {-4, 66, 4.0};
    signed char auto2[3] = {static1[2], -static1[0]};
    unsigned char auto3[2] = {'a'};

    if (auto1[0] != -4 || auto1[1] != 66 || auto1[2] != 4 || auto1[3] || auto1[4])
        return 4;

    if (auto2[0] || auto2[1] != -1 || auto2[2])
        return 5;

    if (auto3[0] != 'a' || auto3[1])
        return 6;

    return 0;
}