int main(void)
{
    /* "30." won't match our regex because it's followed by a letter.
     * It's a preprocessing number but not a valid constant token.
     */
    double foo = 30.e;
    return 4;
}