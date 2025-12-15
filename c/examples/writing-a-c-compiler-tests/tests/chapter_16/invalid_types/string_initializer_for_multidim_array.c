/* A string literal can only initialize a char array.
 * It can't initialize arrays with any other element type, including char[3]
 */
char arr[3][3] = "hello";

int main(void)
{
    return arr[0][2];
}