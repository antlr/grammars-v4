#include <stdio.h>
int main(void)
{
    int x = 10;   // outer 'x' (int)
    printf("Outer x = %d\n", x);
    {
        double x = 3.14;   // inner 'x' shadows outer 'x'
        printf("Inner x = %f\n", x);
        {
            const char *x = "hello";  // another shadowing
            printf("Innermost x = %s\n", x);
        }
        // Back to the double
        printf("Back to inner x = %f\n", x);
    }
    // Back to the original int
    printf("Back to outer x = %d\n", x);
    return 0;
}
