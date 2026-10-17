int main(void)
{
    /* This shouldn't match our floating-point regex because
     * "2."" is followed by a word character.
     * According to the C standard, "2._" is a preprocessing number
     * that can't be converted to a constant.
     */
    return 2._;
}