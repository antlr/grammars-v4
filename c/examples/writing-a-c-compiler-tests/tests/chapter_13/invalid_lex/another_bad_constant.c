int main(void)
{
    /* This shouldn't match our floating-point
     * regex because "1." is followed by a word character
     * According to the C standard, "1.ex" is a preprocessing number
     * that can't be converted into a constant token.
     */
    return 1.ex;
}