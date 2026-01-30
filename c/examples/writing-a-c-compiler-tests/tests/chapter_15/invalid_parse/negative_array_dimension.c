/* It's illegal to declare an array with a negative size.
 * In our implementation, this is a parse error, because we only accept constant literals
 * in array declarators, not constant expressions.
 * In a more complete implementation that allowed constant expressions in declarators
 * (e.g. arr[10 * 20]), this would be
 * a type error rather than a parse error.
 */
int main(void)
{
    int arr[-3];
    return 0;
}