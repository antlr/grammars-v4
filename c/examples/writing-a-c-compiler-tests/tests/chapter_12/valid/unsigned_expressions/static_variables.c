/* Test initializing and updating unsigned global variables */
static unsigned long x = 9223372036854775803ul; // 2^63 - 5

// make sure these are initialized to zero
unsigned long zero_long;
unsigned zero_int;

int main(void)
{
    if (x != 9223372036854775803ul)
        return 0;
    x = x + 10;
    if (x != 9223372036854775813ul)
        return 0;
    if (zero_long || zero_int)
        return 0;
    return 1;
}