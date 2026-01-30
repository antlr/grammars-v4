/* You can't initialize a char array from a string literal
 * that's too long to fit in it.
 * This goes for sub-arrays and static arrays too.*/
char array[3][3] = {"a", "bcde"};

int main(void)
{

    return 0;
}