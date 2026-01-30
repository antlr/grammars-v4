/* This is a type error, but note that because of the grammar rule for
 * declarators in our implementation, it would be a parse error without the parentheses:
 *   int foo(void)[3][4]
 */
int(foo(void))[3][4];