/* String literals can only initialize char arrays,
 * not arrays of other types. This also applies to nested arrays. */
int main(void)
{
    // This is trying to initialize nested array with type
    // unsigned int[2] from a string literal
    unsigned int nested[1][2] = {"a"};
    return 0;
}