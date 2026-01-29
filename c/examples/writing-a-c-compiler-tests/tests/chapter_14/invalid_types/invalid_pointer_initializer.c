int main(void)
{
    /* It's illegal to initializer a pointer with any integer
     * other than a null pointer constant
     * even if it might be a valid memory address
     */
    int *ptr = 140732898195768ul;
    return 0;
}